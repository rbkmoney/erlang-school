-module(chatbots).
-behaviour(gen_server).

-export([start_link/1,init/1,handle_info/2,handle_call/3,handle_cast/2]).

start_link(Name) ->
    gen_server:start_link({local,Name},?MODULE,Name,[]).

init(Name) ->
    {ok, Name, generate_timeout()}.

lol() ->
    lol.

generate_timeout() ->
    1500 + rand:uniform(2500).

handle_info(timeout, State) ->
    rooms:send_message(State, "Hello, I'm " ++ atom_to_list(State)),
    {noreply,State,generate_timeout()}.

%Have to implement

handle_call(_,_,State) ->
    {noreply,State,generate_timeout()}.

handle_cast(_,State) ->
    {noreply,State,generate_timeout()}.
