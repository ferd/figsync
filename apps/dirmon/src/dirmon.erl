-module(dirmon).
-export([track/2, track_remote/3]).
-export([pull/2]).

track(Name, Dir) ->
    case dirmon_tracker_sup:add_tracker(dirmon_monitor, dirmon_data_provider, Name, Dir, root) of
        {ok, _} -> tracked;
        {error, {already_started,_}} -> already_tracked
    end.

track_remote(Name, Dir, Peer) ->
    %% This can probably done by fetching remote info about an absolute
    %% directory, creating a local one, tracking the empty local one, and
    %% then pulling from the remote one.
    case dirmon_tracker_sup:add_tracker(dirmon_monitor, dirmon_data_provider, Name, Dir, {peeranha_erldist, Peer}) of
        {ok, _} -> tracked;
        {error, {already_started,_}} -> already_tracked
    end.

pull(Local, Remote={Node,DbName}) when is_atom(Node) ->
    {ok, LocalDir} = dirmon_data_provider:abs_dir(dirmon_data_provider, Local),
    {ok, RemoteDir} = dirmon_data_provider:abs_dir({dirmon_data_provider, Node}, DbName),
    TmpDir = filename:join(["/tmp", "dirmon", atom_to_list(Local)]),
    peeranha:pull(Local,
                  {peeranha_erldist, Remote},
                  fun(Winner, Key, LocalVal, PeerVal) ->
                      pre_pull(Winner, Key, LocalVal, PeerVal,
                               Remote, TmpDir, LocalDir, RemoteDir)
                  end,
                  fun(Key, Val) ->
                      post_pull(Key, Val,
                                TmpDir, LocalDir)
                  end).

        % peer|local|conflict
%pre_pull(Winner, Key, LocalVal, PeerVal, {Node, Name})
pre_pull(peer, Key, _LocalVal, PeerVal,
         {Node, _Name}, TmpDir, _LocalDir, RemoteDir) ->
    case {dirmon_utils:check_extension(Key), PeerVal} of
        {ignored, _} ->
            skip;
        {accepted, undefined} -> % deleted!
            ok;
        {accepted, {conflict, _}} -> % Rework to skip on failure
            ok = fetch_remote_conflicts(Key, PeerVal, RemoteDir, TmpDir, Node);
        {accepted, _} -> % Rework to skip on failure
            ok = fetch_remote(Key, RemoteDir, TmpDir, Node)
    end;
pre_pull(local, _Key, undefined, _PeerVal,
         _Remote, _TmpDir, _LocalDir, _RemoteDir) ->
    %% Was deleted, stays deleted
    ok;
pre_pull(local, Key, _LocalVal, _PeerVal,
         _Remote, TmpDir, LocalDir, _RemoteDir) ->
    %% Copy everything around to be sure. We can remove this later as
    %% an optimization.
    ok = fetch_local(Key, LocalDir, TmpDir);
pre_pull(conflict, Key, LocalVals, PeerVals,
         {Node, _Name}, TmpDir, LocalDir, RemoteDir) ->
    ok = fetch_local_conflicts(Key, LocalVals, LocalDir, TmpDir),
    ok = fetch_remote_conflicts(Key, PeerVals, RemoteDir, TmpDir, Node).

post_pull(Key, undefined, _Tmp, Local) ->
    %% undefined value = deletion
    file:delete(filename:join([Local, Key]));
post_pull(Key, {conflict, Vals}, Tmp, Local) ->
    %% Find all conflicting things
    %% Does this function call work with unicode?
    FileNames = conflict_filenames(Key, Tmp),
    %% The lengths may vary if we're merging through a partial conflict
    %% resolution, but Vals should always be greater
    true = length(Vals) >= length(FileNames),
    [file:copy(filename:join([Tmp, Name]), filename:join([Local,Name]))
     || Name <- FileNames],
    [file:delete(filename:join([Tmp, Name])) || Name <- FileNames],
    ok;
post_pull(Key, {ok,_Val}, Tmp, Local) ->
    %% Kill all the conflict files, if any
    TmpFileNames = conflict_filenames(Key, Tmp),
    FileNames = conflict_filenames(Key, binary_to_list(Local)),
    [file:delete(filename:join([Local,Path])) || Path <- FileNames],
    %% Move the current thing
    file:copy(filename:join([Tmp, Key]),
             filename:join([Local, Key])),
    file:delete(filename:join([Tmp,Key])),
    %% Clear tmp files
    [file:delete(filename:join([Tmp,Path])) || Path <- TmpFileNames].

fetch_local_conflicts(_, undefined, _, _) -> ok;
fetch_local_conflicts(Key, {ok, Val}, LocalDir, TmpDir) ->
    fetch_local(Key, conflict_suffix(Key, Val), LocalDir, TmpDir);
fetch_local_conflicts(Key, {conflict,List=[_|_]}, LocalDir, TmpDir) ->
    lists:foldl(
        fun(Val, _) ->
            ok = fetch_local(conflict_suffix(Key, Val),
                             conflict_suffix(Key, Val),
                             LocalDir, TmpDir)
        end,
        nostate,
        List).

fetch_remote_conflicts(_, undefined, _, _, _) -> ok;
fetch_remote_conflicts(Key, {ok, Val}, RemoteDir, TmpDir, Node) ->
    fetch_remote(Key, conflict_suffix(Key, Val), RemoteDir, TmpDir, Node);
fetch_remote_conflicts(Key, {conflict, List=[_|_]}, RemoteDir, TmpDir, Node) ->
    lists:foldl(
        fun(Val, _) ->
            fetch_remote(conflict_suffix(Key, Val),
                         conflict_suffix(Key, Val),
                         RemoteDir, TmpDir, Node)
        end,
        nostate,
        List).

fetch_local(Key, LocalDir, TmpDir) ->
    %% Fetch both conflicting files (if any) and the original one
    Conflicts = conflict_filenames(Key, LocalDir),
    [fetch_local(K, K, LocalDir, TmpDir) || K <- [Key | Conflicts]],
    ok.

fetch_local(Key, TmpKey, LocalDir, TmpDir) ->
    Tmp = filename:join([TmpDir, TmpKey]),
    ok = filelib:ensure_dir(Tmp),
    AbsLocal = filename:join(LocalDir, Key),
    {ok, _} = file:copy(AbsLocal, Tmp),
    ok.

fetch_remote(Key, RemoteDir, TmpDir, Node) ->
    fetch_remote(Key, Key, RemoteDir, TmpDir, Node).

fetch_remote(Key, TmpKey, RemoteDir, TmpDir, Node) ->
    %% This bit is non-portable, please fix
    Tmp = filename:join([TmpDir, TmpKey]),
    case skip_remote_fetch(Tmp) of
        true -> ok;
        false ->
            ok = filelib:ensure_dir(Tmp),
            AbsRemote = filename:join(RemoteDir, Key),
            dirmon_utils:copy_remote_to_local(Node, AbsRemote, Tmp)
    end.

conflict_suffix(Key, Hash) ->
    <<Key/binary, ".", Hash/binary, ".conflict">>.

conflict_filenames(Key, Pwd) when is_binary(Pwd) ->
    %% Should probably find why we get this thing in here
    conflict_filenames(Key, binary_to_list(Pwd));
conflict_filenames(Key, Pwd) ->
    filelib:wildcard(binary_to_list(<<Key/binary, "*.conflict">>), Pwd).

skip_remote_fetch(Name) ->
    %% if the file is on disk already in the TMP dir, skip the remote fetch
    filelib:is_regular(Name).
