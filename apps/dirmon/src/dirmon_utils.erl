-module(dirmon_utils).
-export([find_dbs/0, add_db/1]).
-export([check_extension/1]).

find_dbs() ->
    {ok, Path} = application:get_env(dirmon, db_path),
    Name = filename:join(Path, "tracking.consult"),
    %% {Name :: _, UUID :: _  Dir :: string(), Type :: root | normal}
    case file:consult(Name) of
        {ok, ToTrack} -> {ok, ToTrack};
        {error, enoent} -> {error, not_found}
    end.

add_db(Db = {_Name, _UUID, _Dir, _Type}) ->
    {ok, Path} = application:get_env(dirmon, db_path),
    Store = filename:join(Path, "tracking.consult"),
    Existing = case find_dbs() of
        {ok, List} -> List;
        {error, not_found} -> []
    end,
    case lists:member(Db, Existing) of
        true ->
            ok;
        false ->
            {ok, Io} = file:open(Store, [append]),
            io:format(Io, "~p.~n", [Db]),
            file:close(Io)
    end.

check_extension(Path) ->
    Check = lists:any(
        fun(Ext) -> binary:longest_common_suffix([Path, Ext]) =:= byte_size(Ext) end,
        application:get_env(dirmon, ignored_extensions, [])
    ),
    case Check of
        true -> ignored;
        false -> accepted
    end.
