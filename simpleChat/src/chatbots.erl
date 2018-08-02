-module(chatbots).
-behaviour(gen_server).

-export([start_link/1,init/1,handle_info/2,handle_call/3,handle_cast/2]).

start_link(Name) ->
    gen_server:start_link({local,Name},?MODULE,Name,[]).

-spec init(atom()) ->
    {ok, atom(), non_neg_integer()}.
init(Name) ->
    lager:info("Bot ~p initialized",[Name]),
    {ok, Name, generate_timeout()}.

-spec generate_timeout() ->
    non_neg_integer().
generate_timeout() ->
    1500 + rand:uniform(2500).

-spec handle_info(timeout,atom()) ->
    {noreply,atom(),non_neg_integer()}.
handle_info(timeout, Name) ->
    Message = "Hello, I'm " ++ atom_to_list(Name),
    lager:info("Bot ~p sent message ~p",[Name,Message]),
    rooms:send_message(Name, Message),
    {noreply,Name,generate_timeout()}.

%Have to implement
-spec handle_call(_,_,atom()) ->
    {noreply,atom(),non_neg_integer()}.
handle_call(_,_,Name) ->
    {noreply,Name,generate_timeout()}.
-spec handle_cast(_,atom()) ->
    {noreply,atom(),non_neg_integer()}.
handle_cast(_,Name) ->
    {noreply,Name,generate_timeout()}.
