-module(room_manager).

-export([create_room/1]).
-export([delete_room/1]).
-export([room_exists/1]).
-export([room_pid/1]).
-export([rooms/0]).

-spec room_exists(Id :: binary()) ->
    boolean().

room_exists(Id) ->
    case gproc:lookup_local_name({chat_room, Id}) of
        undefined ->
            false;
        _ ->
            true
    end.

-spec create_room(Id :: binary()) ->
    ok | already_exists.

create_room(Id) ->
    case room_exists(Id) of
        false ->
            chat_room:start_link(Id), % Room registers itslef while initializing
            ok;
        true ->
            already_exists
    end.

-spec delete_room(Id :: binary()) ->
    ok | not_found.

delete_room(Id) ->
    case room_exists(Id) of
        false ->
            not_found;
        true ->
            ok = chat_room:stop(Id) % Room unregs itself while terminating
    end.

-spec room_pid(Id :: binary()) ->
    not_found | pid().

room_pid(Id) ->
    case gproc:lookup_local_name({chat_room, Id}) of
        undefined ->
            not_found;
        PID ->
            PID
    end.

-spec rooms() ->
    [binary()].

rooms() ->
    RawRes = raw_room_list(),
    [extract_room(Room) || Room <- RawRes].

raw_room_list() ->
    MatchHead = '_',
    Guard = [],
    Result = ['$$'],
    gproc:select([{MatchHead, Guard, Result}]). % select *

extract_room([{_, _, {chat_room, Id}}, _, _]) ->
    Id.
