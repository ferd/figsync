%%% Track information that will need to be available globally to a
%%% bunch of external processes from other nodes, namely which
%%% absolute directories are being tracked (database *must* store
%%% relative paths to be able to sync across nodes).
%%%
%%% The code currently uses maps, which should be rewritten (when
%%% the VM supports it) to use pattern matching rather than module
%%% function calls.
-module(dirmon_data_provider).
-behaviour(gen_server).

%% API
-export([abs_dir/2,
         abs_dir/3]).

%% Internal API
-export([start_link/1,
         add_abs/3]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {dirs = maps:new()}).

%%%===================================================================
%%% API
%%%===================================================================
abs_dir(Name, DbName) ->
    abs_dir(Name, DbName, 5000).

abs_dir(Name, DbName, Timeout) ->
    gen_server:call(Name, {read_abs_dir, DbName}, Timeout).

%%%===================================================================
%%% Internal API
%%%===================================================================
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

add_abs(Name, Db, Dir) ->
    gen_server:call(Name, {write_abs_dir, Db, Dir}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({read_abs_dir, DbName}, _From, S=#state{dirs=Map}) ->
    case maps:find(DbName, Map) of
        error -> {reply, {error, undefined}, S};
        {ok, Path} -> {reply, {ok, Path}, S}
    end;
handle_call({write_abs_dir, DbName, Path}, _From, S=#state{dirs=Map}) ->
    {reply, ok, S#state{dirs=maps:put(DbName, Path, Map)}};
handle_call(Request, _From, State) ->
    error_logger:warning_msg("mod=~p at=handle_call warning=unexpected_event "
                             "msg=~p", [?MODULE, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("mod=~p at=handle_cast warning=unexpected_event "
                             "msg=~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:warning_msg("mod=~p at=handle_info warning=unexpected_event "
                             "msg=~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
