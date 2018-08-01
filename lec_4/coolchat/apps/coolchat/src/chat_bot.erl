-module(chat_bot).
-author("Kehitt").

-behavior(gen_server).
%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-define(TIMEOUT, rand:uniform(10000)).

start_link(BotName) ->
    gen_server:start_link({local, BotName}, ?MODULE, [BotName], []).

init([BotName | _Args]) ->
    {ok, #{ username => BotName}, ?TIMEOUT}.

handle_call(_, _, State) ->
    {noreply, State, ?TIMEOUT}.

handle_cast(_, State) ->
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State = #{ username := Name}) ->
    chat_room:new_message(Name, "Test from " ++ atom_to_list(Name)),
    {noreply, State, ?TIMEOUT}.