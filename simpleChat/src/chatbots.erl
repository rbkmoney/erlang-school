-module(chatbots).
-behaviour(gen_server).

-export([start_link/1,init/1,handle_info/2,handle_call/3,handle_cast/2]).

start_link(Name) ->
    gen_server:start_link({local,Name},?MODULE,Name,[]).

init(Name) ->
    lager:info("Bot ~p initialized",[Name]),
    {ok, Name, generate_timeout()}.

generate_timeout() ->
    1500 + rand:uniform(2500).

handle_info(timeout, Name) ->
    Message = "Hello, I'm " ++ atom_to_list(Name),
    lager:info("Bot ~p sent message ~p",[Name,Message]),
    rooms:send_message(Name, Message),
    {noreply,Name,generate_timeout()}.

%Have to implement

handle_call(_,_,Name) ->
    {noreply,Name,generate_timeout()}.

handle_cast(_,Name) ->
    {noreply,Name,generate_timeout()}.
