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

%% RUN TESTS RUN
all() -> [boot_up, track_files, track_many_dirs, {group, sync}].
groups() -> [{sync, [],
              [sync_01, sync_02, sync_03, sync_04]}].

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
%% |    | Pull 1 -> 2 | B           | A           | B           |
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
%% |    | Pull 3 -> 1 | (A1,A3).c   | A2          | (A1,A2,A3).c|
%% |    | Del 1.A2    | (A1,A2,A3).c| A2          | (A1,A2,A3).c| * conflict wasn't resolved, gets recreated
%% |    | Del 1.A1    | (A1,A3).c   | A2          | (A1,A2,A3).c|
%% |    | Pull 1 -> 2 | A3          | A2          | (A1,A2,A3).c|
%% |    | Pull 2 -> 3 | A3          | A3          | (A1,A2,A3).c|
%% |    |             | A3          | A3          | A3          |
%% +----+-------------+-------------+-------------+-------------+
%% TODO: Nested directories syncing test

sync_01(Config) ->
    [Dir1, Dir2 | _] = ?config(dirs, Config),
    Data = ?config(data_dir, Config),
    Bindings = [{a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                {b, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                {c, <<"3.gif">>, contents(filename:join(Data, "3.gif"))}],
    init(Dir1, Bindings),
    init(Dir2, []), % noop
    track(dir1, Dir1),
    track(dir2, Dir2),
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
    track(dir2, Dir2),
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
    Bindings = [_={a, <<"1.gif">>, contents(filename:join(Data, "1.gif"))},
                B={b, <<"2.gif">>, contents(filename:join(Data, "2.gif"))},
                _={c, <<"3.gif">>, contents(filename:join(Data, "3.gif"))}],
    init(Dir1, Bindings), 
    init(Dir2, Bindings),
    init(Dir3, [B]),
    track(dir1, Dir1),
    track(dir2, Dir2),
    track(dir3, Dir3),
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings),
    peek(dir3, Dir3, [b], Bindings),
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
    init(Dir2, Bindings),
    init(Dir3, Bindings),
    track(dir1, Dir1),
    track(dir2, Dir2),
    track(dir3, Dir3),
    peek(dir1, Dir1, [a,b,c], Bindings),
    peek(dir2, Dir2, [a,b,c], Bindings),
    peek(dir3, Dir3, [a,b,c], Bindings),
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

%% Possible cases, with failures/interrupts during syncs:
%%  TODO
%%

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

peek(Name, Base, List, Bindings) ->
    %% check that all files are the right ones
    peek_each(Name, Base, List, Bindings),
    %% Check the count is right
    {ok, Listed} = file:list_dir(Base),
    ?assertEqual(length(List), length(Listed)).

peek_each(_Name, _Base, [], []) -> ok;
peek_each(Name, Base, Vars=[], [{_Var, BaseName, _Content} | Bindings]) ->
    %% Test for the absence of a value
    ct:pal("testing abs of ~p",[BaseName]),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    ?until_error(undefined, peeranha:read(Name, BaseName)),
    {error, enoent} = file:read_file(Path),
    peek_each(Name, Base, Vars, Bindings);
peek_each(Name, Base, [Var|Vars], Bindings) ->
    %% Test for the presence of a value
    {value, {Var, BaseName, Content}, NewBindings} = lists:keytake(Var, 1, Bindings),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    ?until_ok(_, peeranha:read(Name, BaseName)),
    {ok, Content} = file:read_file(Path),
    peek_each(Name, Base, Vars, NewBindings).

delete(Base, Var, Bindings) ->
    {Var, BaseName, _Content} = lists:keyfind(Var, 1, Bindings),
    Path = iolist_to_binary(filename:join(Base, BaseName)),
    file:delete(Path).

file(Base, Atom) -> filename:join([Base, atom_to_file(Atom)]).

atom_to_file(a) -> "1.gif";
atom_to_file(b) -> "2.gif";
atom_to_file(c) -> "3.gif".

