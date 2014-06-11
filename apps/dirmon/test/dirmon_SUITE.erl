-module(dirmon_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [].
groups() -> [].

%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP/TEARDOWN %%%
%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    %% Overwrite basic config for database directory
    ok = application:load(dirmon),
    application:set_env(db_path, ?config(priv_dir, Config)),
    Config.

end_per_suite(Config) ->
    ok = application:unload(dirmon),
    Config.

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

%% Scan a directory for the first time and find all the images

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
