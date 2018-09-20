-module(message_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_websocket_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NO_ROOM_REPLY, {error, <<>>, <<"NO ROOM">>, <<>>}).
-define(ALREADY_SUBSCRIBED_REPLY, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(NOT_SUBSCRIBED_REPLY, {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(ALREADY_EXISTS_REPLY, {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).
-define(SUCCESS_REPLY, {success, <<>>, <<>>, <<>>}).

-type error() :: no_room | not_subscribed | already_subscribed | already_exists.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_websocket_message(protocol:source_message(), State :: ws_handler:state()) ->
    [binary()].

handle_websocket_message(Message = {join, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to join room ~p", [Username, Room]),
    NewSubscriptions = case resolve_join_room(Room, Subs) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            ws_handler:send(?SUCCESS_REPLY, self()),
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, [Room | Subs]]),
            [Room | Subs];
        Else ->
            send_error_message(Else),
            Subs
    end,
    NewSubscriptions;

handle_websocket_message(Message = {send_message, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to send ~p to room ~p", [Username, Message, Room]),
    case chatlib:is_in_list(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p sent message ~p to room ~p",[Username, Message, Room]);
        false ->
            send_error_message(not_subscribed)
    end,
    Subs;


handle_websocket_message(Message = {leave, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to leave room ~p", [Username, Room]),
    NewSubscriptions = case chatlib:is_in_list(Room, Subs) of
        true ->
            gproc_ps:unsubscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p left room ~p, he is in rooms ~p now", [Username, Room, lists:delete(Room, Subs)]),
            lists:delete(Room, Subs);
        false ->
            send_error_message(not_subscribed),
            Subs
    end,
    NewSubscriptions;

handle_websocket_message({create, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to create room ~p", [Username, Room]),
    NewSubscriptions = case room_manager:create_room(Room) of
        ok ->
            ws_handler:send(?SUCCESS_REPLY, self()),
            gproc_ps:subscribe(l, Room),
            CreateMessage = {create, Username, <<>>, Room},
            gproc_ps:publish(l, Room, CreateMessage),
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, [Room | Subs]]),
            [Room | Subs];
        already_exists ->
            send_error_message(already_exists),
            Subs
    end,
    NewSubscriptions;

handle_websocket_message(Message = {delete, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to delete room ~p", [Username, Room]),
    NewSubscriptions = case chatlib:is_in_list(Room, Subs) of
        true ->
            gproc_ps:unsubscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            ok = room_manager:delete_room(Room),
            lists:delete(Room, Subs);
        false ->
            send_error_message(not_subscribed),
            Subs
    end,
    NewSubscriptions;

handle_websocket_message(_, #{subscriptions := Subs}) ->
    Subs.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send_error_message(error()) ->
    ok.

send_error_message(no_room) ->
    ws_handler:send(?NO_ROOM_REPLY, self());

send_error_message(already_subscribed) ->
    ws_handler:send(?ALREADY_SUBSCRIBED_REPLY, self());

send_error_message(not_subscribed) ->
    ws_handler:send(?NOT_SUBSCRIBED_REPLY, self());

send_error_message(already_exists) ->
    ws_handler:send(?ALREADY_EXISTS_REPLY, self()).

-spec resolve_join_room(Room :: binary(), Subscriptions :: [binary()]) ->
    ok | no_room | already_subscribed.

resolve_join_room(Room, Subscriptions) ->
    case chatlib:is_in_list(Room, Subscriptions) of % If not already subscribed
        false ->
            case room_manager:room_exists(Room) of
                true ->
                    ok;
                false ->
                    no_room
            end;
        true ->
            already_subscribed
    end.
