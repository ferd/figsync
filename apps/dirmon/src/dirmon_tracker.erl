-module(dirmon_tracker).
-include_lib("kernel/include/file.hrl").
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

-record(state, {ref :: reference(), % file monitor ref
                dir :: binary(), % file monitor dir
                db  :: term()
               }). 

%%%===================================================================
%%% API
%%%===================================================================

start_link(MonitorName, DbName, DbUUID, Dir, Type) ->
    gen_server:start_link(?MODULE, {MonitorName, DbName, DbUUID, Dir, Type}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({MonitorName, DbName, DbUUID, Dir, Type}) ->
    process_flag(trap_exit, true), % wait for ops to finish if possible
    ok = init_db(DbName, DbUUID, Dir, Type),
    ct:pal("Dir: ~p",[Dir]),
    {ok, Ref, BinDir} = file_monitor:automonitor(MonitorName, Dir, []),
    %% Load existing files and dirs
    %% Scan everything
    %% Find SHAs and stuff
    %% Merge the diff?
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
handle_info({file_monitor, Ref, {found, Path, file, _FileInfo, _Files}},
            S=#state{ref=Ref, db=Db}) ->
    %% Found a file
    case maybe_add(Db, Path) of
        added -> error_logger:info_msg("Tracking file ~s", [Path]);
        known -> already_tracked;
        ignored -> ignored
    end,       
    {noreply, S};
handle_info({file_monitor, Ref, {changed, Path, file, _FileInfo, []}},
            S=#state{ref=Ref, db=Db}) ->
    %% Updated file
    update(Db, Path),
    error_logger:info_msg("Detected file update ~s", [Path]),
    {noreply, S};
handle_info({file_monitor, Ref, {errpr, Path, file, enoent}},
            S=#state{ref=Ref, db=Db}) ->
    %% Deleted file
    %% the syncer cannot delete a file in a delete conflict, it must leave
    %% it as is.
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
init_db(Name, UUID, Dir, root) ->
    ok = peeranha:boot(Name, [{type, root}, {dir, Dir}, {uuid, UUID}]),
    dirmon_utils:add_db({Name, UUID, Dir, root});
init_db(Name, UUID, Dir, normal) ->
    %% TODO: handle non-initialized DB that needs to be fetched
    %% from a peer.
    ok = peeranha:boot(Name, [{type, normal}, {dir, Dir}, {uuid, UUID}]),
    dirmon_utils:add_db({Name, UUID, Dir, normal}).

maybe_add(Db, Path) ->
    case dirmon_utils:check_extension(Path) == accepted
      andalso peeranha:read(Db, Path) of
        false -> ignored;
        {ok, _Val} -> already_tracked;
        {conflict, deleted, _} -> already_tracked;
        {conflict, _} -> already_tracked;
        {error, undefined} ->
            Hash = hash_file(Path),
            peeranha:write(Db, Path, Hash),
            added
    end.

update(Db, Path) ->
    %% handle: {internal, File, Hash, ..?} from sync mechanism
    %% or the sync mechanism only leaves the .part or .conflict extensions
    %% and the merge acts as a final write to update
    %% case peeranha:read(Db, Path) of
    %%     {ok, Hash} ->
    %%         ok;
    %%     {conflict, deleted, Values} ->
    %%         case lists:member(Hash, Values) of
    %%             true ->
    %%                 ok;
    %%             false ->
    %%                 peeranha:write(Db, Path, Hash)
    %%         end;
    %%     {conflict, Values} ->
    %%         case lists:member(Hash, Values) of
    %%             true ->
    %%                 ok;
    %%             false ->
    %%                 peeranha:write(Db, Path, Hash)
    %%         end
    %% end.
    peeranha:write(Db, Path, hash_file(Path)).

delete(Db, Path) ->
    peeranha:delete(Db, Path).

hash_file(Path) ->
    %% let's make it human-readable because that may work better
    %% with shasum
    {ok, Data} = file:read_file(Path),
    Hash = crypto:hash(sha, Data),
    iolist_to_binary([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Hash]).

