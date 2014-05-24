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
%% | 06 | Move 2.A=>B | A1,B1       | A1,B1       | N/A         |
%% |    | Pull 2 -> 1 | B2          | A1,B1       | N/A         |
%% |    | Pull 1 -> 2 | B2          | A1,B1       | N/A         |
%% |    |             | B2          | B2          | N/A         |
%% +----+-------------+-------------+-------------+-------------+

%% Possible cases, with failures/interrupts during syncs:
%%  TODO

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE / HELPERS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
