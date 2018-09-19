-module(client_code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NO_ROOM_REPLY, {error, <<>>, <<"NO ROOM">>, <<>>}).
-define(ALREADY_SUBSCRIBED_REPLY, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(NOT_SUBSCRIBED_REPLY, {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(AlREADY_EXISTS_REPLY, {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).
-define(SUCCESS_REPLY, {success, <<>>, <<>>, <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(WebsocketPid) ->
    loop(WebsocketPid, []).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

loop(WebsocketPid, Subscriptions) ->
    Reply = receive
        {websocket, Message} ->
            NewSubscriptions = handle_websocket_message(Message, Subscriptions, WebsocketPid);
        {gproc_ps_event, Event, Message} ->
            NewSubscriptions = Subscriptions,
            handle_event(Message, WebsocketPid);
        stop ->
            NewSubscriptions = Subscriptions,
            stop
    end,
    case Reply of
        stop ->
            stopped;
        _ ->
            loop(WebsocketPid, NewSubscriptions)
    end.

handle_event(Message, WebsocketPid) ->
    ws_handler:send(Message, WebsocketPid).

handle_websocket_message(Message = {join, Username, _, Room}, Subscriptions, WebsocketPid) ->
    ok = lager:info("User ~p wants to join room ~p", [Username, Room]),
    NewSubscriptions = case resolve_join_room(Room, Subscriptions) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            ws_handler:send(?SUCCESS_REPLY, WebsocketPid),
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, [Room | Subscriptions]]),
            [Room | Subscriptions];
        Else ->
            send_error_message(Else, WebsocketPid),
            Subscriptions
    end,
    NewSubscriptions;

handle_websocket_message(Message = {send_message, Username, _, Room}, Subscriptions, WebsocketPid) ->
    ok = lager:info("User ~p wants to send ~p to room ~p", [Username, Message, Room]),
    case is_in_list(Room, Subscriptions) of
        true ->
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p sent message ~p to room ~p",[Username, Message, Room]);
        false ->
            send_error_message(not_subscribed, WebsocketPid)
    end,
    Subscriptions;


handle_websocket_message(Message = {leave, Username, _, Room}, Subscriptions, WebsocketPid) ->
    ok = lager:info("User ~p wants to leave room ~p", [Username, Room]),
    NewSubscriptions = case is_in_list(Room, Subscriptions) of
        true ->
            gproc_ps:unsubscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p left room ~p, he is in rooms ~p now", [Username, Room, lists:delete(Room, Subscriptions)]),
            lists:delete(Room, Subscriptions);
        false ->
            send_error_message(not_subscribed, WebsocketPid),
            Subscriptions
    end,
    NewSubscriptions;

handle_websocket_message({create, Username, _, Room}, Subscriptions, WebsocketPid) ->
    ok = lager:info("User ~p wants to create room ~p", [Username, Room]),
    NewSubscriptions = case room_manager:create_room(Room) of
        ok ->
            ws_handler:send(?SUCCESS_REPLY, WebsocketPid),
            gproc_ps:subscribe(l, Room),
            CreateMessage = {create, Username, <<>>, Room},
            gproc_ps:publish(l, Room, CreateMessage),
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, [Room | Subscriptions]]),
            [Room | Subscriptions];
        already_exists ->
            send_error_message(already_exists, WebsocketPid),
            Subscriptions
    end,
    NewSubscriptions;

handle_websocket_message(Message = {delete, Username, _, Room}, Subscriptions, WebsocketPid) ->
    ok = lager:info("User ~p wants to delete room ~p", [Username, Room]),
    NewSubscriptions = case is_in_list(Room, Subscriptions) of
        true ->
            gproc_ps:unsubscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            ok = room_manager:delete_room(Room),
            lists:delete(Room, Subscriptions);
        false ->
            send_error_message(not_subscribed, WebsocketPid),
            Subscriptions
    end,
    NewSubscriptions;

handle_websocket_message(_, Subscriptions, WebsocketPid) ->
    Subscriptions.

send_error_message(no_room, WebsocketPid) ->
    ws_handler:send(?NO_ROOM_REPLY, WebsocketPid);

send_error_message(already_subscribed, WebsocketPid) ->
    ws_handler:send(?ALREADY_SUBSCRIBED_REPLY, WebsocketPid);

send_error_message(not_subscribed, WebsocketPid) ->
    ws_handler:send(?NOT_SUBSCRIBED_REPLY, WebsocketPid);

send_error_message(already_exists, WebsocketPid) ->
    ws_handler:send(?AlREADY_EXISTS_REPLY, WebsocketPid).

resolve_join_room(Room, Subscriptions) ->
    case is_in_list(Room, Subscriptions) of % If not already subscribed
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

is_in_list(Item, List) ->
    ok = lager:info("Searching for ~p in ~p", [Item, List]),
    case [Elem || Elem <- List, Elem == Item] of
        [] ->
            false;
        _ ->
            true
    end.
