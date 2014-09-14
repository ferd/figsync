-module(dirmon_tracker).
-include_lib("kernel/include/file.hrl").
-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ref :: reference(), % file monitor ref
                dir :: binary(), % file monitor dir
                db  :: term()
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MonitorName, ProviderName, DbName, DbUUID, Dir, Type) ->
    gen_server:start_link(
        ?MODULE,
        {MonitorName, ProviderName, DbName, DbUUID, Dir, Type},
        []
    ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({MonitorName, ProviderName, DbName, DbUUID, Dir, Type}) ->
    process_flag(trap_exit, true), % wait for ops to finish if possible
    ok = init_db(DbName, DbUUID,
                 filename:join([dirmon_utils:db_dir(), DbName]),
                 Type, Dir),
    {ok, Ref, BinDir} = file_monitor:automonitor(MonitorName, Dir, []),
    %% Tell the data provider that we're tracking this abs path.
    dirmon_data_provider:add_abs(ProviderName, DbName, BinDir),
    %% Somehow automonitors don't accept configs so we set it by hand
    file_monitor:set_interval(MonitorName, dirmon_utils:monitor_interval()),
    %% Because we set it by hand, there's a delay before we can get
    %% the demanded interval. For the first time, force a manual poll
    %% to schedule things at the right time.
    MonitorName ! poll,
    {ok, #state{ref=Ref, dir=BinDir, db=DbName}}.

handle_call(Request, _From, State) ->
    error_logger:warning_msg("mod=~p at=handle_call warning=unexpected_event "
                             "msg=~p", [?MODULE, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("mod=~p at=handle_cast warning=unexpected_event "
                             "msg=~p", [?MODULE, Msg]),
    {noreply, State}.

%handle_info({file_monitor, _Ref,
%%% initial found:
%% {file_monitor, Ref,
%%  {found, <<"dir">>, directory,
%%    #file_info{},
%%    [{added, <<"file1">>},
%%     {added, <<"dir1">>},
%%     ...
%%     {added, <<"fileN">>}]}}
%% {file_monitor, Ref, 
%%   {found, <<"dir/file1">>, file,
%%    #file_info{},
%%    []}}
%% {file_monitor, Ref,
%%   {found, <<"dir/subdir">>}, directory,
%%    #file_info{},
%%    [...]}}
%%
%%% File update:
%% {file_monitor, Ref,
%%   {changed, <<"dir/file1">>, file,
%%    #file_info{},
%%   []}}
%%
%%% Adding file:
%% {file_monitor, Ref,
%%  {found, <<"dir/file2">>, file,
%%   #file_info{},
%%   []}}
%%
%%% Adding directory
%% {file_monitor, Ref,
%%  {found, <<"dir/subdir">>, directory,
%%   #file_info{},
%%   []}}
%%
%%% Deleting file
%% {file_monitor, Ref,
%%  {error, <<"dir/file2">>, file, enoent}}
%% {file_monitor, Ref,
%%  {error, <<"dir/subdir">>, directory, enoent}}
%%
%%% Ignore:
%%% {file_monitor, Ref,
%%%  {changed, <<"topleveldir">>, directory},
%%%  #file_info{},
%%%  [NewFile] | []}
handle_info({file_monitor, Ref, {found, Dir, directory, _FileInfo, Files}},
            S=#state{ref=Ref}) ->
    %% Found a directory
    error_logger:info_msg("Found directory ~s with files ~p",[Dir,Files]),
    {noreply, S};
handle_info({file_monitor, Ref, {changed, _, directory, _FileInfo, _Files}},
            S=#state{ref=Ref}) ->
    %% Modification within a directory
    {noreply, S};
handle_info({file_monitor, Ref, {error, _, directory, enoent}},
            S=#state{ref=Ref}) ->
    %% Deleted directory
    {noreply, S};
handle_info({file_monitor, Ref, {found, AbsPath, file, _FileInfo, _Files}},
            S=#state{ref=Ref, db=Db, dir=Dir}) ->
    %% Found a file
    Path = make_relative(Dir, AbsPath),
    case maybe_add(Db, Dir, Path) of
        added -> error_logger:info_msg("Tracking file ~p", [Path]);
        known -> already_tracked;
        ignored -> ignored
    end,
    {noreply, S};
handle_info({file_monitor, Ref, {changed, AbsPath, file, _FileInfo, []}},
            S=#state{ref=Ref, db=Db, dir=Dir}) ->
    %% Updated file
    Path = make_relative(Dir, AbsPath),
    update(Db, Dir, Path),
    error_logger:info_msg("Detected file update ~s", [Path]),
    {noreply, S};
handle_info({file_monitor, Ref, {error, AbsPath, file, enoent}},
            S=#state{ref=Ref, db=Db, dir=Dir}) ->
    %% Deleted file
    %% the syncer cannot delete a file in a delete conflict, it must leave
    %% it as is.
    Path = make_relative(Dir, AbsPath),
    delete(Db, Path),
    error_logger:info_msg("Stopped tracking file ~s", [Path]),
    {noreply, S};
handle_info(Info, State) ->
    error_logger:warning_msg("mod=~p at=handle_info warning=unexpected_event "
                             "msg=~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_db(Name, UUID, Dir, Type, ToMonitor) ->
    %% TODO: handle non-initialized DB that needs to be fetched
    %% from a peer (type normal)
    ok = peeranha:boot(Name, [{type, Type}, {dir, Dir}, {uuid, UUID}]),
    dirmon_utils:add_db({Name, UUID, ToMonitor, Type}).

maybe_add(Db, Dir, Path) ->
    case dirmon_utils:check_extension(Path) == accepted
      andalso peeranha:read(Db, Path) of
        false -> ignored;
        {ok, _Val} -> known;
        {conflict, deleted, _} -> known;
        {conflict, _} -> known;
        {error, undefined} ->
            Hash = hash_file(filename:join(Dir, Path)),
            peeranha:write(Db, Path, Hash),
            added
    end.

update(Db, Dir, Path) ->
    %% handle: {internal, File, Hash, ..?} from sync mechanism
    %% or the sync mechanism only leaves the .part or .conflict extensions
    %% and the merge acts as a final write to update. However, because
    %% the write is merged in the DB by peeranha already, we have to
    %% avoid overwriting things for no reason
    Hash = hash_file(filename:join(Dir,Path)),
    case peeranha:read(Db, Path) of
        {ok, Hash} ->
            ok;
        {ok, _} ->
            peeranha:write(Db, Path, Hash);
        {conflict, deleted, Values} ->
            case lists:member(Hash, Values) of
                true -> ok;
                false -> peeranha:write(Db, Path, Hash)
            end;
        {conflict, Values} ->
            case lists:member(Hash, Values) of
                true -> ok;
                false -> peeranha:write(Db, Path, Hash)
            end
    end.

delete(Db, Path) ->
    peeranha:delete(Db, Path).

hash_file(Path) ->
    %% let's make it human-readable because that may work better
    %% with shasum
    {ok, Data} = file:read_file(Path),
    Hash = crypto:hash(sha, Data),
    iolist_to_binary([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Hash]).

make_relative(Dir, Path) when is_binary(Dir), is_binary(Path) ->
    Offset = byte_size(Dir),
    Tot = byte_size(Path),
    binary:part(Path, {Tot, (Offset-Tot)+1}). % +1 drops a separating '/'
