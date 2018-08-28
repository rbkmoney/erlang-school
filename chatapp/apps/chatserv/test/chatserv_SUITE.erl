-module(chatserv_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(AWAIT_DELAY, 1000).
-define(AWAIT_RETRIES, 5).

all() ->
    [
        join_room,
        set_name,
        send_message
    ].

init_per_suite(C) ->
    application:ensure_all_started(chatserv),
    application:ensure_all_started(chatcli),
    C.

end_per_suite(C) ->
    application:stop(chatcli),
    application:stop(chatserv),
    C.

init_per_testcase(_, C) ->
    C.

end_per_testcase(_, C) ->
    C.

join_room(_) ->
    ok = chatcli_client:join_room(1),
    ok = chatcli_client:await_response(?AWAIT_DELAY, ?AWAIT_RETRIES).

set_name(_) ->
    ok = chatcli_client:set_name(1, "Test"),
    ok = chatcli_client:await_response(?AWAIT_DELAY, ?AWAIT_RETRIES).

send_message(_) ->
    ok = chatcli_client:send_message(1, "Test Message"),
    _ = timer:sleep(1000),
    true = chatcli_client:has_message(1, "Test", "Test Message").

exchange_messages(_) -> ok.