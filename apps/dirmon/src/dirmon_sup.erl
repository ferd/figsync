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
    DataName = dirmon_data_provider,
    {ok, {{rest_for_one, 1, 60},
     [{file_monitor,
       {file_monitor, start_link, [{local,MonName}, []]},
       permanent, 1000, worker, [file_monitor]},
      {dist_data_provider,
       {dirmon_data_provider, start_link, [{local,DataName}]},
       permanent, 1000, worker, [dirmon_data_provider]},
      {file_tracker_sup,
       {dirmon_tracker_sup, start_link, [MonName, DataName]},
       permanent, 5000, supervisor, [dirmon_tracker_sup]}]}}.
