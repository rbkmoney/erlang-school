-module(chatserv_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(AWAIT_DELAY, 50).
-define(AWAIT_RETRIES, 10).

-define(HOST, "localhost").
-define(IP, {0, 0, 0, 0}).
-define(PORT, 8888).

all() ->
    [
        solo_happy,
        solo_happy,
        duo_exchange
    ].

%%
%% Initializers
%%

init_per_suite(C) ->
    ok = application:load(chatserv),

    ok = application:set_env(chatserv, listen_ip, ?IP),
    ok = application:set_env(chatserv, listen_port, ?PORT),

    {ok, Apps1} = application:ensure_all_started(chatserv),
    {ok, Apps2} = application:ensure_all_started(chatcli),

    [{apps, Apps1 ++ Apps2}|C].

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps, C)].

%%
%% solo_happy
%%

solo_happy(_) ->
    This = self(),
    C1 = make_client(fun() -> This ! {recieve_message, self()} end),

    ok = chatcli_client:join_room(C1, 1),
    ok = chatcli_client:set_name(C1, 1, "Test"),
    ok = chatcli_client:send_message(C1, 1, "Test Message"),

    ok = receive_messages([C1]),

    stop_client(C1).

solo_fails(_) ->
    C1 = make_client(fun() -> ok end),

    room_not_joined = chatcli_client:set_name(C1, 1, "Test"),
    room_not_joined = chatcli_client:send_message(C1, 1, "Test Message"),

    room_does_not_exist = chatcli_client:join_room(C1, 999),

    ok = chatcli_client:join_room(C1, 1),
    room_already_joined = chatcli_client:join_room(C1, 1),

    stop_client(C1).

duo_exchange(_) ->
    This = self(),
    C1 = make_client(fun() -> This ! {recieve_message, self()} end),
    C2 = make_client(fun() -> This ! {recieve_message, self()} end),

    ok = chatcli_client:join_room(C1, 1),

    ok = chatcli_client:join_room(C2, 1),
    ok = chatcli_client:join_room(C2, 2),

    ok = chatcli_client:set_name(C1, 1, "TestUser1"),

    ok = chatcli_client:set_name(C2, 1, "TestUser2"),
    ok = chatcli_client:set_name(C2, 2, "TestUser2"),

    ok = chatcli_client:send_message(C1, 1, "Test Message From User 1"),
    ok = chatcli_client:send_message(C2, 1, "Test Message From User 2"),

    ok = receive_messages([C1, C2]),

    ok = chatcli_client:send_message(C2, 2, "A message from User 2 user 1 should not see"),

    ok = receive_messages([C2]),
    timeout = receive_messages([C1]),

    stop_client(C1),
    stop_client(C2).

%%
%% Helpers
%%
receive_messages([]) -> ok;
receive_messages([H|T]) ->
    receive
        {recieve_message, H} -> receive_messages(T)
        after 2000 -> timeout
    end.

stop_client(Pid) ->
    chatcli_sup:stop_client(Pid).

make_client(MessageCB) ->
    {ok, Pid} = chatcli_sup:start_client(?HOST, ?PORT, MessageCB),

    Pid.