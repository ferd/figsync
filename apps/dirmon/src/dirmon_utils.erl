-module(dirmon_utils).
-export([find_dbs/0, add_db/1]).
-export([db_dir/0, monitor_interval/0]).
-export([check_extension/1, copy_remote_to_local/3, stream_file/3]).
-define(BATCH, 10240). % 10kb

find_dbs() ->
    {ok, Path} = application:get_env(dirmon, db_path),
    Name = filename:join(Path, "tracking.consult"),
    %% {Name :: _, UUID :: _ , Dir :: string(), Type :: root | normal}
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

db_dir() ->
    {ok, Path} = application:get_env(dirmon, db_path),
    Path.

monitor_interval() ->
    {ok, Interval} = application:get_env(dirmon, monitor_interval),
    Interval.

check_extension(Path) ->
    Check = lists:any(
        fun(Ext) -> binary:longest_common_suffix([Path, Ext]) =:= byte_size(Ext) end,
        application:get_env(dirmon, ignored_extensions, [])
    ),
    case Check of
        true -> ignored;
        false -> accepted
    end.

copy_remote_to_local(Node, Remote, Local) ->
    %% We're gonna stream shit! This piece entirely relies on the
    %% A -> B ordering of message across nodes. We want to stream because
    %% some gifs are quite big and if we can avoid buffering it in memory,
    %% let's do so.
    Receiver = self(),
    Ref = make_ref(),
    rpc:cast(Node, ?MODULE, stream_file, [Receiver, Ref, Remote]),
    stream_to_disk(Ref, Local).

stream_file(To, Ref, File) ->
    case file:open(File, [read,raw,binary]) of
        {error, enoent} ->
            To ! {Ref, enoent};
        {ok, F} ->
            To ! {Ref, self()}, % let the other monitor us
            stream_file(To, Ref, F, 0)
    end.

stream_file(To, Ref, F, Offset) ->
    case file:pread(F, Offset, ?BATCH) of
        eof ->
            file:close(F),
            To ! {Ref, eof};
        {ok, Bytes} ->
            To ! {Ref, Offset, Bytes},
            stream_file(To, Ref, F, Offset+?BATCH)
    end.

stream_to_disk(Ref, Path) ->
    receive
        {Ref, enoent} -> 
            %% remote file doesn't exist -- possibly a partial conflict
            %% resolution, or maybe a race.
            missing;
        {Ref, Pid} when is_pid(Pid) ->
            %% Open the file, monitor the remote end to avoid
            %% hanging, then loop in.
            {ok, F} = file:open(Path, [write,raw]),
            Monitor = erlang:monitor(process, Pid),
            stream_to_disk(Ref, F, Monitor)
    after 15000 ->
        error(remote_end_not_streaming)
    end.

stream_to_disk(Ref, F, Monitor) ->
    receive
        {Ref, eof} ->
            erlang:demonitor(Monitor, [flush]),
            file:close(F);
        {Ref, Offset, Bytes} ->
            file:pwrite(F, Offset, Bytes),
            ok = stream_to_disk(Ref, F, Monitor);
        {'DOWN', Monitor, process, _Pid, Reason} ->
            error({remote, Reason})
    end.
