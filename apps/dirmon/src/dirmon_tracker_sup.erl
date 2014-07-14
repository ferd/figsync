-module(dirmon_tracker_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, add_tracker/4]).

%% Supervisor callbacks
-export([init/1]).

start_link(Monitor) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Monitor).

add_tracker(Monitor, Name, Dir, Type) ->
    supervisor:start_child(
        ?MODULE,
        child(Monitor, Name, uuid:get_v4(weak), Dir, Type)
    ).

init(Monitor) ->
    %% Find config of where DB files go
    %% Start or connect to open DB
    Dbs = case dirmon_utils:find_dbs() of
        {ok, Data} -> Data;
        {error, not_found} -> []
    end,
    %% Spawn one worker per configured dir
    {ok, {{one_for_one, 10, 60},
         [child(Monitor, Name, UUID, Dir, Type)
          || {Name, UUID, Dir, Type} <- Dbs]}}.

child(Monitor, Name, UUID, Dir, Type) ->
    {{file_tracker,Dir},
     {dirmon_tracker, start_link, [Monitor, Name, UUID, Dir, Type]},
     permanent, 1000, worker, [dirmon_tracker]}.
