-module(dirmon_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% Macros to allow retrying until file monitor polls without waiting the
%% max amount of time unless we really need to.
-define(until_ok(Var, Action),
    Var = (fun() ->
        Fn=fun(_, 0) -> error(timeout)
           ;  (F, T) -> case Action of
                            {ok, X} -> X;
                            _ -> timer:sleep(10), F(F, T-10)
                        end
        end,
        Fn(Fn, 500)
    end)()).
-define(until_conflict(Var, Action),
    Var = (fun() ->
        Fn=fun(_, 0) -> error(timeout)
           ;  (F, T) -> case Action of
                            {conflict, X} -> X;
                            _ -> timer:sleep(10), F(F, T-10)
                        end
        end,
        Fn(Fn, 500)
    end)()).
-define(until_error(Var, Action),
    Var = (fun() ->
        Fn=fun(_, 0) -> error(timeout)
           ;  (F, T) -> case Action of
                            {error, X} -> X;
                            _ -> timer:sleep(10), F(F, T-10)
                        end
        end,
        Fn(Fn, 500)
    end)()).
-define(until_not_error(Var, Action),
    Var = (fun() ->
        Fn=fun(_, 0) -> error(timeout)
           ;  (F, T) -> case Action of
                            {error, _} -> timer:sleep(10), F(F, T-10);
                            X -> X
                        end
        end,
        Fn(Fn, 500)
    end)()).

%% RUN TESTS RUN
all() -> [boot_up, track_files, track_many_dirs, {group, sync}].
groups() -> [{sync, [],
              [sync_01, sync_02, sync_03, sync_04, sync_05, sync_06,
               sync_07, sync_08, sync_09]}].

%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP/TEARDOWN %%%
%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(Name, Config) ->
    Priv = ?config(priv_dir, Config),
    Data = ?config(data_dir, Config),
    [Db, Img1, Img2, Img3] = init_dirs(Data, Priv, atom_to_list(Name)),
    %% Overwrite basic config for database directory
    ok = application:load(dirmon),
    application:set_env(dirmon, db_path, Db),
    application:set_env(dirmon, monitor_interval, 100),
    {ok, Started} = application:ensure_all_started(dirmon),
    [{started, Started}, {dirs, [Img1,Img2,Img3]} | Config].

end_per_testcase(_Name, Config) ->
    [application:stop(App) || App <- lists:reverse(?config(started, Config))],
    ok = application:unload(dirmon),
    Config.

init_dirs(_Data, Root, Test) ->
    Paths = [Db = filename:join([Root, Test, "db/"]),
             Img1=filename:join([Root, Test, "img1/"]),
             Img2=filename:join([Root, Test, "img2/"]),
             Img3=filename:join([Root, Test, "img3/"])],
    ok = filelib:ensure_dir(filename:join([Db, ".ensured"])),
    ok = filelib:ensure_dir(filename:join([Img1, ".ensured"])),
    ok = filelib:ensure_dir(filename:join([Img2, ".ensured"])),
    ok = filelib:ensure_dir(filename:join([Img3, ".ensured"])),
    Paths.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

%% Scan a directory for the first time and find all the images
boot_up(Config) ->
    %% Should have no child
    [] = supervisor:which_children(dirmon_tracker_sup),
    %% Track a dir
    Dir = hd(?config(dirs, Config)),
    tracked = dirmon:track("some_name", Dir),
    already_tracked = dirmon:track("some_name", Dir),
    %% Should have a child.
    [_] = supervisor:which_children(dirmon_tracker_sup),
    %% Shut down, restart app, should have a child
    [application:stop(App) || App <- lists:reverse(?config(started, Config))],
    {ok, _} = application:ensure_all_started(dirmon),
    %% already_tracked because some other process took over
    %% when booting
    already_tracked = dirmon:track("some_name", Dir),
    [_] = supervisor:which_children(dirmon_tracker_sup).

%% scan a directory, wait a bit, see that the files are there
%% in the DB
track_files(Config) ->
    %% Copy the data to the priv dir
    Dir = hd(?config(dirs, Config)),
    Data = ?config(data_dir, Config),
    [file:copy(filename:join([Data,Img]), filename:join([Dir, Img]))
        || Img <- ["1.gif","2.gif","3.gif"]],
    AbsImg1 = list_to_binary(filename:join(Dir, "1.gif")),
    AbsImg2 = list_to_binary(filename:join(Dir, "2.gif")),
    AbsImg3 = list_to_binary(filename:join(Dir, "3.gif")),
    Img1 = <<"1.gif">>,
    Img2 = <<"2.gif">>,
    Img3 = <<"3.gif">>,
    tracked = dirmon:track("some_name", Dir),
    %% See that the files are tracked
    ?until_ok(_, peeranha:read("some_name", Img1)),
    ?until_ok(_, peeranha:read("some_name", Img1)),
    ?until_ok(Hash1, peeranha:read("some_name", Img2)),
    ?until_ok(_, peeranha:read("some_name", Img3)),
    ?until_error(undefined,
                 peeranha:read("some_name", list_to_binary(filename:join(Dir, "ignore.part")))),
    %% Move a file (copy -> delete) and see that it worked
    {ok, _} = file:copy(AbsImg1, AbsImg2),
    ok = file:delete(AbsImg1),
    ?until_error(undefined, peeranha:read("some_name", Img1)),
    ?until_ok(Hash2, peeranha:read("some_name", Img2)),
    ct:pal("Hash1: ~p, Hash2: ~p", [Hash1,Hash2]),
    ?assertNotEqual(Hash1, Hash2).

%% scan a directory, wait a bit, see that the files are there
%% in the DB, then monitor a second one, see that there's
%% no conflict.
track_many_dirs(Config) ->
    [Dir1,Dir2|_] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    [file:copy(filename:join([Data,Img]), filename:join([Dir1, Img]))
        || Img <- ["1.gif","2.gif","3.gif"]],
    AbsImg1 = list_to_binary(filename:join(Dir1, "1.gif")),
    AbsImg2 = list_to_binary(filename:join(Dir1, "2.gif")),
    AbsImg3 = list_to_binary(filename:join(Dir1, "3.gif")),
    AbsImgA = list_to_binary(filename:join(Dir2, "A.gif")),
    AbsImgB = list_to_binary(filename:join(Dir2, "B.gif")),
    AbsImgC = list_to_binary(filename:join(Dir2, "C.gif")),
    Img1 = <<"1.gif">>,
    Img2 = <<"2.gif">>,
    Img3 = <<"3.gif">>,
    ImgA = <<"A.gif">>,
    ImgB = <<"B.gif">>,
    ImgC = <<"C.gif">>,
    tracked = dirmon:track("some_name", Dir1),
    %% See that the files are tracked
    ?until_ok(H1, peeranha:read("some_name", Img1)),
    ?until_ok(H2, peeranha:read("some_name", Img2)),
    ?until_ok(H3, peeranha:read("some_name", Img3)),
    %% Second dir
    tracked = dirmon:track("other_name", Dir2),
    {ok,_} = file:copy(AbsImg1,AbsImgC),
    {ok,_} = file:copy(AbsImg2,AbsImgB),
    {ok,_} = file:copy(AbsImg3,AbsImgA),
    ?until_ok(H3, peeranha:read("other_name", ImgA)),
    ?until_ok(H2, peeranha:read("other_name", ImgB)),
    ?until_ok(H1, peeranha:read("other_name", ImgC)),
    ?until_error(undefined, peeranha:read("some_name", ImgC)),
    ?until_error(undefined, peeranha:read("other_name", Img1)).

%% Scan multiple directories for the first time and find all the images
%% Without confusing them

%% It is possible to sync two directories together.
%%
%% Possible cases, assuming no failures:
%% +----+-------------------------------------------------------+
%% |    |             |     State before action takes place     |
%% | ## |   Action    |-----------------------------------------|
%% |    |             | Directory 1 | Directory 2 | Directory 3 |
%% +----+-----------------------------------------+-------------+
%% | 01 | Pull 1 -> 2 | A,B,C       | Ø           | N/A         |
%% |    |             | A,B,C       | A,B,C       | N/A         |
%% +----+-------------+-------------+-------------+-------------+
%% | 02 | Pull 1 -> 2 | A,C         | B           | N/A         |
%% |    | Pull 2 -> 1 | A,C         | A,B,C       | N/A         |
%% |    |             | A,B,C       | A,B,C       | N/A         |
%% +----+-------------+-------------+-------------+-------------+
%% | 03 | Delete 1.B  | A,B,C       | A,B,C       | B           |
%% |    | Pull 1 -> 2 | A,C         | A,B,C       | B           |
%% |    | Pull 2 -> 3 | A,C         | A,C         | B           |
%% |    |             | A,C         | A,C         | A,C         |
%% +----+-------------+-------------+-------------+-------------+
%% | 04 | Delete 1.A  | A,B,C       | A,B,C       | A,B,C       |
%% |    | Delete 2.B  | B,C         | A,B,C       | A,B,C       |
%% |    | Delete 3.C  | B,C         | A,C         | A,B,C       |
%% |    | Pull 1 -> 2 | B,C         | A,C         | A,B         |
%% |    | Pull 3 -> 2 | B,C         | C           | A,B         |
%% |    | Pull 2 -> 1 | B,C         | Ø           | A,B         |
%% |    | Pull 2 -> 3 | Ø           | Ø           | A,B         |
%% |    |             | Ø           | Ø           | Ø           |
%% +----+-------------+-------------+-------------+-------------+
%% | 05 | Add 1.A     | Ø           | Ø           | Ø           |
%% |    | Pull 1 -> 2 | A           | Ø           | Ø           |
%% |    | Move 1.A=>B | A           | A           | Ø           |
%% |    | Pull 2 -> 3 | B           | A           | Ø           |
%% |    | Pull 1 -> 3 | B           | A           | A           |
%% |    | Move 2.A=>B | B           | A           | B           |
%% |    | Pull 1 -> 2 | B           | B           | B           |
%% |    |             | B           | B           | B           |
%% +----+-------------+-------------+-------------+-------------+
%% | 06 | Move 1.A=>B | A1,B1       | A1,B1       | N/A         |
%% |    | Pull 2 -> 1 | B2          | A1,B1       | N/A         |
%% |    | Pull 1 -> 2 | B2          | A1,B1       | N/A         |
%% |    |             | B2          | B2          | N/A         |
%% +----+-------------+-------------+-------------+-------------+
%% | 07 | Add 1.A1    | Ø           | Ø           | Ø           |
%% |    | Add 2.A2    | A1          | Ø           | Ø           |
%% |    | Pull 2 -> 1 | A1          | A2          | Ø           |
%% |    | Add 3.A3    | A1.c,A2.c   | A2          | Ø           |
%% |    | Pull 3 -> 1 | A1.c,A2.c   | A2          | A3          |
%% |    | Pull 1 -> 3 | (A1,A2,A3).c| A2          | A3          |
%% |    | Del 1.A2    | (A1,A2,A3).c| A2          | (A1,A2,A3).c| 
%% |    | Pull 3 -> 1 | (A1,A3).c   | A2          | (A1,A2,A3).c| // partial conflict resolution -- we won't see the diff
%% |    | Del 1.A1    | (A1,A3).c   | A2          | (A1,A2,A3).c| %% make a case of syncing with files missing
%% |    | Move A3c=>A3| A3.c        | A2          | (A1,A2,A3).c|
%% |    | Pull 1 -> 2 | A3          | A2          | (A1,A2,A3).c|
%% |    | Pull 2 -> 3 | A3          | A3          | (A1,A2,A3).c|
%% |    |             | A3          | A3          | A3          |
%% +----+-------------+-------------+-------------+-------------+
%% | 08 | Add 1.A     | Ø           | Ø           | N/A         |
%% |    | Add 2.A     | A           | A           | N/A         |
%% |    | Pull 1 -> 2 | A           | A           | N/A         |
%% |    | Pull 2 -> 1 | A           | A           | N/A         | * no conflict
%% |    |             | A           | A           | N/A         | * no conflict
%% +----+-------------+-------------+-------------+-------------+
%% | 09 | Add 1.A1    | Ø           | Ø           | Ø           |
%% |    | Add 2.A2    | A1          | Ø           | Ø           |
%% |    | Pull 2 -> 1 | A1          | A2          | Ø           |
%% |    | Add 3.A3    | A1.c,A2.c   | A2          | Ø           |
%% |    | Pull 3 -> 1 | A1.c,A2.c   | A2          | A3          |
%% |    | Del 1.A1    | (A1,A2,A3).c| A2          | A3          | 
%% |    | Pull 1 -> 3 | (A2,A3).c   | A2          | A3          |
%% |    | Del 1.A3    | (A2,A3).c   | A2          | (A2,A3).c   | 
%% |    | Del 3.A2    | A2.c        | A2          | (A2,A3).c   | 
%% |    | Pull 3 -> 1 | A2.c        | A2          | A3.c        | 
%% |    | Pull 1 -> 3 | A2.c        | A2          | A3.c        | % conflicted but sync'd on that fact
%% |    | Move A2c=>A2| A2.c        | A2          | A3.c        | % conflicted but sync'd on that fact 
%% |    | Pull 3 -> 1 | A2          | A2          | A3.c        |
%% |    | Pull 1 -> 3 | A2          | A2          | A3.c        |
%% |    | Pull 1 -> 2 | A2          | A2          | A2          |
%% |    |             | A2          | A2          | A2          |
%% +----+-------------+-------------+-------------+-------------+
%%
%% TODO: Nested directories syncing test
%% TODO: Test deleting all conflict files without the original source
%%       file
%% TODO: Test overwriting the source file in a conflict group
%%       and make sure overwriting a conflict file base flushes
%%       the *.conflict files attached
%% TODO: Test deleting the source file in a conflict group and make
%%       sure this removed the *.conflict files attached

sync_01(Config) ->
    [Dir1, Dir2 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [{a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                {b, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                {c, <<"3.gif">>, contents(filename:join(Data, "3.gif"))}],
    init(Dir1, Bindings),
    init(Dir2, []), % noop
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings).

sync_02(Config) ->
    [Dir1, Dir2 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [A={a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                B={b, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                C={c, <<"3.gif">>, contents(filename:join(Data, "3.gif"))}],
    init(Dir1, [A,C]), 
    init(Dir2, [B]),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    peek(dir1, Dir1, [a,c], Bindings),
    peek(dir2, Dir2, [b], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings),
    dirmon:pull(dir1, {node(), dir2}), % pull from dir2 into dir1
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings).

sync_03(Config) ->
    [Dir1, Dir2, Dir3 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [{a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                {b, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                {c, <<"3.gif">>, contents(filename:join(Data, "3.gif"))}],
    %% Need a more complex setup since the spec mentions existing shared
    %% files.
    init(Dir1, []),
    init(Dir2, []),
    init(Dir3, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    track(dir3, Dir3, dir2),
    add(Dir1, b, Bindings),
    peek(dir1, Dir1, [b], Bindings), % sync
    dirmon:pull(dir3, {node(), dir1}),
    add(Dir1, a, Bindings),
    add(Dir1, c, Bindings),
    peek(dir1, Dir1, [a,b,c], Bindings), % sync
    dirmon:pull(dir2, {node(), dir1}),
    %% SETUP OVER
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings),
    peek(dir3, Dir3, [b], Bindings),
    timer:sleep(500), % shit fix for race condition on file watches
    delete(Dir1, b, Bindings), % delete 1.b
    peek(dir1, Dir1, [a,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings),
    peek(dir3, Dir3, [b], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a,c], Bindings),
    peek(dir2, Dir2, [a,c], Bindings),
    peek(dir3, Dir3, [b], Bindings),
    dirmon:pull(dir3, {node(), dir2}), % pull from dir2 into dir3
    peek(dir1, Dir1, [a,c], Bindings),
    peek(dir2, Dir2, [a,c], Bindings),
    peek(dir3, Dir3, [a,c], Bindings),
    ok.

sync_04(Config) ->
    [Dir1, Dir2, Dir3 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [{a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                {b, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                {c, <<"3.gif">>, contents(filename:join(Data, "3.gif"))}],
    init(Dir1, Bindings), 
    init(Dir2, []),
    init(Dir3, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    track(dir3, Dir3, dir1),
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [], Bindings),
    dirmon:pull(dir2, {node(), dir1}),
    dirmon:pull(dir3, {node(), dir1}),
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings),
    peek(dir3, Dir3, [a,b,c], Bindings),
    %% Figure out error here where a race condition happens:
    %% we didn't have the time for the file monitor to detect b in Dir2
    %% before it swiftly got deleted (the sync test sees the data already due
    %% to the merge working) but then later doesn't ever report the file
    %% going away due to never seeing it appear first.
    timer:sleep(500), % shit fix for file monitor race condition
    delete(Dir1, a, Bindings), % delete 1.a
    delete(Dir2, b, Bindings), % delete 2.b
    delete(Dir3, c, Bindings), % delete 3.c
    peek(dir1, Dir1, [b,c], Bindings),
    peek(dir2, Dir2, [a,c], Bindings),
    peek(dir3, Dir3, [a,b], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [b,c], Bindings),
    peek(dir2, Dir2, [c], Bindings),
    peek(dir3, Dir3, [a,b], Bindings),
    dirmon:pull(dir2, {node(), dir3}), % pull from dir3 into dir2
    peek(dir1, Dir1, [b,c], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [a,b], Bindings),
    dirmon:pull(dir1, {node(), dir2}), % pull from dir2 into dir1
    peek(dir1, Dir1, [], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [a,b], Bindings),
    dirmon:pull(dir3, {node(), dir2}), % pull from dir2 into dir3
    peek(dir1, Dir1, [], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [], Bindings),
    ok.

sync_05(Config) ->
    [Dir1, Dir2, Dir3 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [{a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                {b, <<"2.gif">>, contents(filename:join(Data, "1.gif"))}],
    init(Dir1, []), 
    init(Dir2, []),
    init(Dir3, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    track(dir3, Dir3, dir2),
    peek(dir1, Dir1, [], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [], Bindings),
    add(Dir1, a, Bindings),
    peek(dir1, Dir1, [a], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a], Bindings),
    peek(dir2, Dir2, [a], Bindings),
    peek(dir3, Dir3, [], Bindings),
    move(Dir1, a, b, Bindings),
    peek(dir1, Dir1, [b], Bindings),
    peek(dir2, Dir2, [a], Bindings),
    peek(dir3, Dir3, [], Bindings),
    dirmon:pull(dir3, {node(), dir2}), % pull from dir2 into dir3
    peek(dir1, Dir1, [b], Bindings),
    peek(dir2, Dir2, [a], Bindings),
    peek(dir3, Dir3, [a], Bindings),
    dirmon:pull(dir3, {node(), dir1}), % pull from dir1 into dir3
    peek(dir1, Dir1, [b], Bindings),
    peek(dir2, Dir2, [a], Bindings),
    peek(dir3, Dir3, [b], Bindings),
    move(Dir2, a, b, Bindings),
    peek(dir1, Dir1, [b], Bindings),
    peek(dir2, Dir2, [b], Bindings),
    peek(dir3, Dir3, [b], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [b], Bindings),
    peek(dir2, Dir2, [b], Bindings),
    peek(dir3, Dir3, [b], Bindings),
    ok.

sync_06(Config) ->
    [Dir1, Dir2 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [A1={a1, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                B1={b1, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                 _={b2, <<"2.gif">>, contents(filename:join(Data, "1.gif"))}],
    init(Dir1, [A1,B1]),
    init(Dir2, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    peek(dir1, Dir1, [a1,b1], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a1,b1], Bindings),
    peek(dir2, Dir2, [a1,b1], Bindings),
    move(Dir1, a1, b2, Bindings),
    peek(dir1, Dir1, [b2], Bindings),
    peek(dir2, Dir2, [a1,b1], Bindings),
    dirmon:pull(dir1, {node(), dir2}), % pull from dir2 into dir1
    peek(dir1, Dir1, [b2], Bindings),
    peek(dir2, Dir2, [a1,b1], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [b2], Bindings),
    peek(dir2, Dir2, [b2], Bindings),
    ok.

sync_07(Config) ->
    [Dir1, Dir2, Dir3 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Content1 = contents(filename:join(Data, "1.gif")),
    Content2 = contents(filename:join(Data, "2.gif")),
    Content3 = contents(filename:join(Data, "3.gif")),
    Bindings = [{a1, <<"1.gif">>, Content1},
                {a2, <<"1.gif">>, Content2},
                {a3, <<"1.gif">>, Content3},
                {a1c, <<"1.gif.", (hash_data(Content1))/binary, ".conflict">>, Content1},
                {a2c, <<"1.gif.", (hash_data(Content2))/binary, ".conflict">>, Content2},
                {a3c, <<"1.gif.", (hash_data(Content3))/binary, ".conflict">>, Content3}],
    init(Dir1, []),
    init(Dir2, []),
    init(Dir3, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    track(dir3, Dir3, dir2),
    peek(dir1, Dir1, [], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [], Bindings),
    add(Dir1, a1, Bindings),
    add(Dir2, a2, Bindings),
    peek(dir1, Dir1, [a1], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [], Bindings),
    dirmon:pull(dir1, {node(), dir2}), % pull from dir2 into dir1
    peek(dir1, Dir1, [a1c,a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [], Bindings),
    add(Dir3, a3, Bindings),
    peek(dir1, Dir1, [a1c,a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3], Bindings),
    dirmon:pull(dir1, {node(), dir3}), % pull from dir3 into dir1
    peek(dir1, Dir1, [a1c,a2c,a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3], Bindings),
    dirmon:pull(dir3, {node(), dir1}), % pull from dir1 into dir3
    peek(dir1, Dir1, [a1c,a2c,a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a1c,a2c,a3c], Bindings),
    timer:sleep(500), % shit fix for file monitor race condition
    delete(Dir1, a2c, Bindings),
    peek(dir1, Dir1, [a1c,a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a1c,a2c,a3c], Bindings),
    dirmon:pull(dir1, {node(), dir3}), % pull from dir3 into dir1
    peek(dir1, Dir1, [a1c,a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a1c,a2c,a3c], Bindings),
    timer:sleep(500), % shit fix for file monitor race condition
    delete(Dir1, a1c, Bindings),
    peek(dir1, Dir1, [a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a1c,a2c,a3c], Bindings),
    timer:sleep(500), % shit fix for file monitor race condition
    move(Dir1, a3c, a3, Bindings),
    peek(dir1, Dir1, [a3], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a1c,a2c,a3c], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a3], Bindings),
    peek(dir2, Dir2, [a3], Bindings),
    peek(dir3, Dir3, [a1c,a2c,a3c], Bindings),
    dirmon:pull(dir3, {node(), dir2}), % pull from dir2 into dir3
    peek(dir1, Dir1, [a3], Bindings),
    peek(dir2, Dir2, [a3], Bindings),
    peek(dir3, Dir3, [a3], Bindings),
    hooray.

sync_08(Config) ->
    [Dir1, Dir2 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Content1 = contents(filename:join(Data, "1.gif")),
    Bindings = [{a1, <<"1.gif">>, Content1}],
    init(Dir1, []),
    init(Dir2, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    peek(dir1, Dir1, [], Bindings),
    peek(dir2, Dir2, [], Bindings),
    add(Dir1, a1, Bindings),
    add(Dir2, a1, Bindings),
    peek(dir1, Dir1, [a1], Bindings),
    peek(dir2, Dir2, [a1], Bindings),
    dirmon:pull(dir2, {node(), dir1}), % pull from dir1 into dir2
    peek(dir1, Dir1, [a1], Bindings),
    peek(dir2, Dir2, [a1], Bindings),
    dirmon:pull(dir1, {node(), dir2}), % pull from dir2 into dir1
    peek(dir1, Dir1, [a1], Bindings),
    peek(dir2, Dir2, [a1], Bindings),
    ok.

sync_09(Config) ->
    [Dir1, Dir2, Dir3 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Content1 = contents(filename:join(Data, "1.gif")),
    Content2 = contents(filename:join(Data, "2.gif")),
    Content3 = contents(filename:join(Data, "3.gif")),
    Bindings = [{a1, <<"1.gif">>, Content1},
                {a2, <<"1.gif">>, Content2},
                {a3, <<"1.gif">>, Content3},
                {a1c, <<"1.gif.", (hash_data(Content1))/binary, ".conflict">>, Content1},
                {a2c, <<"1.gif.", (hash_data(Content2))/binary, ".conflict">>, Content2},
                {a3c, <<"1.gif.", (hash_data(Content3))/binary, ".conflict">>, Content3}],
    init(Dir1, []),
    init(Dir2, []),
    init(Dir3, []),
    track(dir1, Dir1),
    track(dir2, Dir2, dir1),
    track(dir3, Dir3, dir2),
    peek(dir1, Dir1, [], Bindings),
    peek(dir2, Dir2, [], Bindings),
    peek(dir3, Dir3, [], Bindings),
    add(Dir1, a1, Bindings),
    add(Dir2, a2, Bindings),
    peek(dir1, Dir1, [a1], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [], Bindings),
    dirmon:pull(dir1, {node(), dir2}), % pull from dir2 into dir1
    peek(dir1, Dir1, [a1c,a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [], Bindings),
    add(Dir3, a3, Bindings),
    peek(dir1, Dir1, [a1c,a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3], Bindings),
    dirmon:pull(dir1, {node(), dir3}), % pull from dir3 into dir1
    peek(dir1, Dir1, [a1c,a2c,a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3], Bindings),
    timer:sleep(500),
    delete(Dir1, a1c, Bindings),
    peek(dir1, Dir1, [a2c,a3c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3], Bindings),
    dirmon:pull(dir3, {node(), dir1}), % pull from dir3 into dir1
    peek(dir1, Dir1, [a2c,a3c], Bindings), 
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a2c,a3c], Bindings),
    timer:sleep(500),
    delete(Dir1, a3c, Bindings),
    peek(dir1, Dir1, [a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a2c,a3c], Bindings),
    timer:sleep(500),
    delete(Dir3, a2c, Bindings),
    peek(dir1, Dir1, [a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3c], Bindings),
    dirmon:pull(dir1, {node(), dir3}), % pull from dir3 into dir1
    peek(dir1, Dir1, [a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3c], Bindings),
    dirmon:pull(dir3, {node(), dir1}), % pull from dir1 into dir3
    peek(dir1, Dir1, [a2c], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3c], Bindings),
    move(Dir1, a2c, a2, Bindings),
    peek(dir1, Dir1, [a2], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3c], Bindings),
    dirmon:pull(dir1, {node(), dir3}), % pull from dir3 into dir1
    peek(dir1, Dir1, [a2], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a3c], Bindings),
    dirmon:pull(dir3, {node(), dir1}), % pull from dir1 into dir3
    peek(dir1, Dir1, [a2], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a2], Bindings),
    dirmon:pull(dir2, {node(), dir3}), % pull from dir3 into dir2
    peek(dir1, Dir1, [a2], Bindings),
    peek(dir2, Dir2, [a2], Bindings),
    peek(dir3, Dir3, [a2], Bindings),
    ok.








%% Possible cases, with failures/interrupts during syncs:
%%  TODO
%% - find deletions on restart?

%% Notes:
%% File monitor needs to monitor a directory, and then all the files in it
%% one by one apparently. returns {file_monitor, Ref, {changed, Dir, Type, FileInfo, Ops}
%% where Ops is a list of {Action, File}, Action :: added | deleted.
%% If Ops = empty, the file's content itself changed.
%%
%%
%% Should use automonitor to get files tracked automatically.
%% New files get the {file_monitor, Ref, {found, Path,Tpe, FileInfo, []}} msg.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE / HELPERS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
contents(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Bin.

init(_Dest, []) -> ok;
init(Dest, [{_Var, Name, Content} | Tail]) ->
    file:write_file(filename:join(Dest, Name), Content),
    init(Dest, Tail).

track(Name, Path) ->
    dirmon:track(Name, Path).

track(Name, Path, From) ->
    dirmon:track_remote(Name, Path, {node(), From}).

peek(Name, Base, List, Bindings) ->
    %% check that all files are the right ones
    peek_each(Name, Base, List, Bindings).
    %% Check that all files match the bindings
    %{ok, Listed} = file:list_dir(Base),
    %?assert(lists:all(fun(Path) -> 

peek_each(_Name, _Base, [], []) -> ok;
peek_each(Name, Base, Vars=[], [{_Var, BaseName, Content} | Bindings]) ->
    %% Test for the absence of a value
    ct:pal("testing abs of ~p",[_Var]),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    try
        ?until_error(undefined, peeranha:read(Name, BaseName)),
        {error, enoent} = file:read_file(Path)
    catch
        error:timeout -> % Maybe it exists from a move and the content differs
            try
                ?until_ok(_, peeranha:read(Name, BaseName)),
                {ok, Bin} = file:read_file(Path),
                ?assertNotEqual(Content, Bin)
            catch
                error:timeout -> % Maybe it's a conflicting file!
                    case filename:extension(Path) of
                        <<".conflict">> ->
                            %% we don't check for DB presence, more than one conflict
                            %% may exist. We just look for absence of file on disk.
                            {ok, BinConflict} = file:read_file(Path),
                            ?assertNotEqual(Content, BinConflict);
                        _ ->
                            %% Conflicting original file
                            ?until_conflict(_, peeranha:read(Name, BaseName))
                    end
            end
    end,
    peek_each(Name, Base, Vars, Bindings);
peek_each(Name, Base, [Var|Vars], Bindings) ->
    %% Test for the presence of a value
    ct:pal("testing pres of ~p",[Var]),
    {value, {Var, BaseName, Content}, NewBindings} = lists:keytake(Var, 1, Bindings),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    case filename:extension(Path) of
        <<".conflict">> ->
            Origin = unconflict_name(BaseName),
            ?until_conflict(_, peeranha:read(Name, Origin)), % we don't stop tracking
            ct:pal("DIR: ~p",[file:list_dir(Base)]),
            {ok, Content} = file:read_file(Path);
        _ ->
            ?until_ok(_, peeranha:read(Name, BaseName)),
            {ok, Content} = file:read_file(Path)
    end,
    peek_each(Name, Base, Vars, NewBindings).

delete(Base, Var, Bindings) ->
    {Var, BaseName, _Content} = lists:keyfind(Var, 1, Bindings),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    file:delete(Path).

add(Base, Var, Bindings) ->
    {Var, BaseName, Content} = lists:keyfind(Var, 1, Bindings),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    file:write_file(Path, Content).

move(Base, VarFrom, VarTo, Bindings) ->
    %% Planned content has to be the same to make the move legal. Then we
    %% just delete the old file and create the new one.
    {VarFrom, _BaseName, Content} = lists:keyfind(VarFrom, 1, Bindings),
    {VarTo, _NewName, Content} = lists:keyfind(VarTo, 1, Bindings),
    delete(Base, VarFrom, Bindings),
    add(Base, VarTo, Bindings).

file(Base, Atom) -> filename:join([Base, atom_to_file(Atom)]).

atom_to_file(a) -> "1.gif";
atom_to_file(b) -> "2.gif";
atom_to_file(c) -> "3.gif".

unconflict_name(BaseName) ->
    OriginSize = byte_size(BaseName) - 50,
    <<Origin:OriginSize/binary, _Dot, _Hash:40/binary, ".conflict">> = BaseName,
    Origin.

hash_data(Data) ->
    Hash = crypto:hash(sha, Data),
    iolist_to_binary([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Hash]).
