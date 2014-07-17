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
            
all() -> [boot_up, track_files].
groups() -> [].

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

init_dirs(Data, Root, Test) ->
    Paths = [Db = filename:join([Root, Test, "db/"]),
             Img1=filename:join([Root, Test, "img1/"]),
             Img2=filename:join([Root, Test, "img2/"]),
             Img3=filename:join([Root, Test, "img3/"])],
    ok = filelib:ensure_dir(filename:join([Db, ".ensured"])),
    ok = filelib:ensure_dir(filename:join([Img1, ".ensured"])),
    ok = filelib:ensure_dir(filename:join([Img2, ".ensured"])),
    ok = filelib:ensure_dir(filename:join([Img3, ".ensured"])),
    {ok, Names} = file:list_dir(Data),
    [file:copy(filename:join([Data,Name]),
               filename:join([Img1, Name])) || Name <- Names],
    [file:copy(filename:join([Data,Name]),
               filename:join([Img2, Name])) || Name <- Names],
    [file:copy(filename:join([Data,Name]),
               filename:join([Img3, Name])) || Name <- Names],
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
    Dir = hd(?config(dirs, Config)),
    Img1 = list_to_binary(filename:join(Dir, "1.gif")),
    Img2 = list_to_binary(filename:join(Dir, "2.gif")),
    Img3 = list_to_binary(filename:join(Dir, "3.gif")),
    tracked = dirmon:track("some_name", Dir),
    %% See that the files are tracked
    ?until_ok(_, peeranha:read("some_name", Img1)),
    ?until_ok(_, peeranha:read("some_name", Img1)),
    ?until_ok(Hash1, peeranha:read("some_name", Img2)),
    ?until_ok(_, peeranha:read("some_name", Img3)),
    ?until_error(undefined,
                 peeranha:read("some_name", list_to_binary(filename:join(Dir, "ignore.part")))),
    %% Move a file (copy -> delete) and see that it worked
    {ok, _} = file:copy(Img1, Img2),
    ok = file:delete(Img1),
    ?until_error(undefined, peeranha:read("some_name", Img1)),
    ?until_ok(Hash2, peeranha:read("some_name", Img2)),
    ?assertNotEqual(Hash1, Hash2).


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
%% |    | Pull 1 -> 2 | A,C         | A,B,C       | N/A         |
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
