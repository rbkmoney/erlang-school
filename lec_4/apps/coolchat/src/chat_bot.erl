-module(chat_bot).
-author("Kehitt").

-define(TIMEOUT, rand:uniform(10000)).
-type state() :: #{
username => string()
}.

%% gen_server
-behavior(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-spec start_link(string()) ->
    {ok, pid()} | {error, _}.
start_link(BotName) ->
    gen_server:start_link(?MODULE, [BotName], []).

-spec init(list()) ->
    {ok, state(), timeout()}.
init([BotName | _Args]) ->
    {ok, #{username => BotName}, ?TIMEOUT}.

-spec handle_call(any(), any(), state()) ->
    {noreply, state(), timeout()}.
handle_call(_, _, State) ->
    {noreply, State, ?TIMEOUT}.

-spec handle_cast(any(), state()) ->
    {noreply, state(), timeout()}.
handle_cast(_, State) ->
    {noreply, State, ?TIMEOUT}.

-spec handle_info(timeout, state()) ->
    {noreply, state(), timeout()}.
handle_info(timeout, State = #{username := Name}) ->
    chat_room:new_message(Name, "Test from " ++ Name),
    {noreply, State, ?TIMEOUT}.