-module(chat_server_message_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NO_ROOM_REPLY, {error, <<>>, <<"NO ROOM">>, <<>>}).
-define(ALREADY_SUBSCRIBED_REPLY, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(NOT_SUBSCRIBED_REPLY, {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(ALREADY_EXISTS_REPLY, {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type error() :: no_room | not_subscribed | already_subscribed | already_exists.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_message(library_protocol:source_message(), State :: chat_server_ws_handler:state()) ->
    [binary()].

handle_message(Message = {join, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to join room ~p", [Username, Room]),
    case resolve_join_room(Room, Subs) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            NewSubs = [Room | Subs],
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            NewSubs;
        Else ->
            send_error_message(Else),
            Subs
    end;

handle_message(Message = {send_message, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to send ~p to room ~p", [Username, Message, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p sent message ~p to room ~p",[Username, Message, Room]);
        false ->
            send_error_message(not_subscribed)
    end,
    Subs;


handle_message(Message = {leave, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to leave room ~p", [Username, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:unsubscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            NewSubs = lists:delete(Room, Subs),
            ok = lager:info("User ~p left room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            NewSubs;
        false ->
            send_error_message(not_subscribed),
            Subs
    end;

handle_message({create, Username, _, Room}, #{subscriptions := Subs}) ->
    ok = lager:info("User ~p wants to create room ~p", [Username, Room]),
    case chat_server_room_manager:create_room(Room) of
        ok ->
            gproc_ps:subscribe(l, Room),
            CreateMessage = {create, Username, <<>>, Room},
            gproc_ps:publish(l, Room, CreateMessage),
            NewSubs = [Room | Subs],
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            NewSubs;
        already_exists ->
            send_error_message(already_exists),
            Subs
    end;

handle_message(Message = {delete, Username, _, Room}, #{subscriptions := Subs}) ->
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

handle_message(_, #{subscriptions := Subs}) ->
    Subs.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send_error_message(error()) ->
    ok.

send_error_message(no_room) ->
    chat_server_ws_handler:send(?NO_ROOM_REPLY, self());

send_error_message(already_subscribed) ->
    chat_server_ws_handler:send(?ALREADY_SUBSCRIBED_REPLY, self());

send_error_message(not_subscribed) ->
    chat_server_ws_handler:send(?NOT_SUBSCRIBED_REPLY, self());

send_error_message(already_exists) ->
    chat_server_ws_handler:send(?ALREADY_EXISTS_REPLY, self()).

-spec resolve_join_room(Room :: binary(), Subscriptions :: [binary()]) ->
    ok | no_room | already_subscribed.

resolve_join_room(Room, Subscriptions) ->
    case lists:member(Room, Subscriptions) of % If not already subscribed
        false ->
            case chat_server_room_manager:room_exists(Room) of
                true ->
                    ok;
                false ->
                    no_room
            end;
        true ->
            already_subscribed
    end.
