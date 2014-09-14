-module(dirmon).
-export([track/2, track_remote/3]).
-export([pull/2]).

track(Name, Dir) ->
    case dirmon_tracker_sup:add_tracker(dirmon_monitor, dirmon_data_provider, Name, Dir, root) of
        {ok, _} -> tracked;
        {error, {already_started,_}} -> already_tracked
    end.

track_remote(_Name, _Dir, _Peer) ->
    %% TODO
    %% This can probably done by fetching remote info about an absolute
    %% directory, creating a local one, tracking the empty local one, and
    %% then pulling from the remote one.
    error(not_implemented).

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
pre_pull(peer, Key, _LocalVal, _PeerVal,
         {Node, _Name}, TmpDir, _LocalDir, RemoteDir) ->
    %% This bit is non-portable, please fix
    Tmp = filename:join([TmpDir, Key]),
    ok = filelib:ensure_dir(Tmp),
    AbsRemote = filename:join(RemoteDir, Key),
    case dirmon_utils:check_extension(Key) of
        ignored ->
            skip;
        accepted -> % Rework to skip on failure
            ok = dirmon_utils:copy_remote_to_local(Node, AbsRemote, Tmp)
    end;
pre_pull(local, Key, _LocalVal, _PeerVal,
         _Remote, TmpDir, LocalDir, _RemoteDir) ->
    %% Copy everything around to be sure. We can remove this later as
    %% an optimization.
    Tmp = filename:join([TmpDir, Key]),
    ok = filelib:ensure_dir(Tmp),
    AbsLocal = filename:join(LocalDir, Key),
    file:copy(AbsLocal, Tmp);
pre_pull(conflict, _Key, _LocalVal, _PeerVal,
         {_Node, _Name}, _TmpDir, _LocalDir, _RemoteDir) ->
    error(todo).

post_pull(Key, _Val, Tmp, Local) ->
   %% naive wrong impl to make a few tests pass. To be refined.
   file:copy(filename:join([Tmp, Key]),
             filename:join([Local, Key])).

