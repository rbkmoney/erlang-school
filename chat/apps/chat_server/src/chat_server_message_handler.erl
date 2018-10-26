-module(chat_server_message_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_event  /2]).
-export([handle_message/2]).

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
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:publish(l, Room, Message),
            ok = lager:info("User ~p sent message ~p to room ~p",[Username, Message, Room]),
            {ok, Subs};
        false ->
            {error, not_joined}
    end;


handle_message(Message = {leave, Username, Room}, Subs) ->
    case lists:member(Room, Subs) of
        true ->
            ok = lager:debug("Unsubscribing ~p from room ~p", [Username, Room]),
            gproc_ps:publish(l, Room, Message),
            gproc_ps:unsubscribe(l, Room),
            NewSubs = lists:delete(Room, Subs),
            ok = lager:info("User ~p left room ~p, he is in rooms ~p now", [Username, Room, NewSubs]),
            {ok, NewSubs};
        false ->
            {error, not_joined}
    end;

handle_message(Message = {create, Username, Room}, Subs) ->
    case lists:member(Room, Subs) of
        false ->
            case chat_server_room_manager:create_room(Room) of
                ok ->
                    ok = lager:info("Created room ~p, subscribing ~p to it", [Room, Username]),
                    gproc_ps:subscribe(l, Room),
                    gproc_ps:publish(l, Room, Message),
                    NewSubs = [Room | Subs],
                    {ok, NewSubs};
                already_exists ->
                    {error, already_exists}
            end;
        true ->
            {error, already_exists}
    end;

handle_message(Message = {delete, Username, Room}, Subs) ->
    case lists:member(Room, Subs) of
        true ->
            ok = lager:debug("Publishing delete room ~p message, origin: ~p", [Room, Username]),
            gproc_ps:publish(l, Room, Message), % Alerting everyone
            _ = chat_server_room_manager:delete_room(Room), % Don't we care?
            {ok, Subs};
        false ->
            {error, not_joined}
    end;

handle_message(_, Subs) ->
    Subs.

-spec handle_event(term(), subscribers()) ->
    {library_protocol:message() | undefined, subscribers()}.

handle_event({gproc_ps_event, Room, Message = {delete, _, _}}, Subs) ->
    case lists:member(Room, Subs) of
        true ->
            gproc_ps:unsubscribe(l, Room),
            {Message, lists:delete(Room, Subs)};
        false ->
            {Message, Subs}
    end;

handle_event({gproc_ps_event, _Room, Message}, Subs) ->
    {Message, Subs};

handle_event(_, Subs) ->
    {undefined, Subs}.

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
