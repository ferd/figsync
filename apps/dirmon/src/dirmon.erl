-module(dirmon).
-export([track/2, track_remote/3]).

track(Name, Dir) ->
    case dirmon_tracker_sup:add_tracker(dirmon_monitor, Name, Dir, root) of
        {ok, _} -> tracked;
        {error, {already_started,_}} -> already_tracked
    end.

track_remote(Name, Dir, Peer) ->
    %% TODO
    error(not_implemented).

