-module(rooms).
-behaviour(gen_server).

-export([send_message/2,get_messages/0]).
-export([handle_cast/2,handle_call/3,init/1,start_link/0]).


start_link() ->
    gen_server:start_link({local,rooms},?MODULE,undefined,[]).

init(undefined) ->
    {ok,[]}.

send_message(Username,Message) ->
    gen_server:cast(?MODULE, {send,{Username,Message,erlang:localtime()}}).

get_messages() ->
    gen_server:call(?MODULE,{get}).

handle_cast({send,Message},State) ->
    {noreply,[Message | State]}.

handle_call({get},_From,State) ->
    {reply,State,State}.
