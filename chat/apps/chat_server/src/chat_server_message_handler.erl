-module(chat_server_message_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_message    /2]).
-export([handle_gproc_event/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([subscribers/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type room()  ::  library_protocol:room().
-type error() :: library_protocol:error().
-type subscribers() ::  [room()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_message(library_protocol:message(), Subs :: subscribers()) ->
    {ok, subscribers()} | error().

handle_message(Message = {join, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to join room ~p", [Username, Room]),
    case resolve_join_room(Room, Subs) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            NewSubs = [Room | Subs],
            ok = lager:debug("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            {ok, NewSubs};
        Else ->
            {error, Else}
    end;

handle_message(Message = {{message, _}, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to send ~p to room ~p", [Username, Message, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p sent message ~p to room ~p",[Username, Message, Room]),
            {ok, Subs};
        false ->
            {error, not_joined}
    end;


handle_message(Message = {leave, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to leave room ~p", [Username, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            gproc_ps:unsubscribe(l, Room),
            NewSubs = lists:delete(Room, Subs),
            ok = lager:info("User ~p left room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            {ok, NewSubs};
        false ->
            {error, not_joined}
    end;

handle_message(Message = {create, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to create room ~p", [Username, Room]),
    case chat_server_room_manager:create_room(Room) of
        ok ->
            gproc_ps:subscribe(l, Room),
            gproc_ps:publish(l, Room, Message),
            NewSubs = [Room | Subs],
            ok = lager:info("User ~p joined room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            {ok, NewSubs};
        already_exists ->
            {error, already_exists}
    end;

handle_message(Message = {delete, Username, Room}, Subs) ->
    ok = lager:info("User ~p wants to delete room ~p", [Username, Room]),
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message), % Alerting everyone
            ok = chat_server_room_manager:delete_room(Room),
            NewSubs = lists:delete(Room, Subs),
            {ok, NewSubs};
        false ->
            {error, not_joined}
    end;

handle_message(_, Subs) ->
    Subs.

-spec handle_gproc_event({gproc_ps_event, room(), library_protocol:message()}, subscribers()) ->
    subscribers().

handle_gproc_event({gproc_ps_event, Room, Message = {delete, _, _}}, Subs) ->
    ok = lager:debug("gproc_ps_event: ~p", [Message]),
    gproc_ps:unsubscribe(l, Room),
    lists:delete(Room, Subs);

handle_gproc_event({gproc_ps_event, _Room, Message}, Subs) ->
    ok = lager:debug("gproc_ps_event: ~p", [Message]),
    Subs.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

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
