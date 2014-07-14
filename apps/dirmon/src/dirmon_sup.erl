-module(dirmon_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MonName = dirmon_monitor,
    {ok, {{rest_for_one, 1, 60},
     [{file_monitor, {file_monitor, start_link, [{local,MonName}, []]},
       permanent, 1000, worker, [file_monitor]},
      {file_tracker_sup, {dirmon_tracker_sup, start_link, [MonName]},
       permanent, 5000, supervisor, [dirmon_tracker_sup]}]}}.
