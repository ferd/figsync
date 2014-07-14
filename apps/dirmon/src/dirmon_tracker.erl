-module(dirmon_tracker).

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MonitorName, DbName, DbUUID, Dir, Type) ->
    gen_server:start_link(?MODULE, {MonitorName, DbName, DbUUID, Dir, Type}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({MonitorName, DbName, DbUUID, Dir, Type}) ->
    ok = init_db(DbName, DbUUID, Dir, Type),
    file_monitor:monitor_dir(MonitorName, Dir, []),
    %% Load existing files and dirs
    %% Scan everything
    %% Find SHAs and stuff
    %% Merge the diff?
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_db(Name, UUID, Dir, root) ->
    ok = peeranha:boot(Name, [{type, root}, {dir, Dir}, {uuid, UUID}]),
    dirmon_utils:add_db({Name, UUID, Dir, root});
init_db(Name, UUID, Dir, normal) ->
    %% TODO: handle non-initialized DB that needs to be fetched
    %% from a peer.
    ok = peeranha:boot(Name, [{type, normal}, {dir, Dir}, {uuid, UUID}]),
    dirmon_utils:add_db({Name, UUID, Dir, normal}).

