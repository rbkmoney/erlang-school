-module(chat_server_message_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type error() :: not_exists | not_subscribed | already_joined | already_exists.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_message(library_protocol:decoded(), Subs :: chat_server_ws_handler:state()) ->
    chat_server_ws_handler:state().

handle_message(Message = {join, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to join room ~p", [Username, Room]),
    case resolve_join_room(Room, Subs) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            NewSubs = [Room | Subs],
            ok = lager:debug("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            NewSubs;
        Else ->
            send_error_message(Else),
            Subs
    end;

handle_message(Message = {{message, _}, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to send ~p to room ~p", [Username, Message, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p sent message ~p to room ~p",[Username, Message, Room]);
        false ->
            send_error_message(not_subscribed)
    end,
    Subs;


handle_message(Message = {leave, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to leave room ~p", [Username, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            gproc_ps:unsubscribe(l, Room),
            NewSubs = lists:delete(Room, Subs),
            ok = lager:info("User ~p left room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            NewSubs;
        false ->
            send_error_message(not_subscribed),
            Subs
    end;

handle_message(Message = {create, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to create room ~p", [Username, Room]),
    case chat_server_room_manager:create_room(Room) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            NewSubs = [Room | Subs],
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            NewSubs;
        already_exists ->
            send_error_message(already_exists),
            Subs
    end;

handle_message(Message = {delete, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to delete room ~p", [Username, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            gproc_ps:unsubscribe(l, Room),
            ok = chat_server_room_manager:delete_room(Room),
            lists:delete(Room, Subs);
        false ->
            send_error_message(not_subscribed),
            Subs
    end;

handle_message(_, Subs) ->
    Subs.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send_error_message(error()) ->
    ok.

send_error_message(not_exists) ->
    chat_server_ws_handler:send({error, not_exists}, self());

send_error_message(already_joined) ->
    chat_server_ws_handler:send({error, already_joined}, self());

send_error_message(not_subscribed) ->
    chat_server_ws_handler:send({error, not_joined}, self());

send_error_message(already_exists) ->
    chat_server_ws_handler:send({error, already_exists}, self()).

-spec resolve_join_room(Room :: binary(), Subscriptions :: [binary()]) ->
    ok | not_exists | already_joined.

resolve_join_room(Room, Subscriptions) ->
    case lists:member(Room, Subscriptions) of % If not already subscribed
        false ->
            case chat_server_room_manager:room_exists(Room) of
                true ->
                    ok;
                false ->
                    not_exists
            end;
        true ->
            already_joined
    end.
