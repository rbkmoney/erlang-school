-module(rooms).
-behaviour(gen_server).

-export([send_message/2,get_messages/0, get_last_messages/1]).
-export([handle_cast/2,handle_call/3,init/1,start_link/0]).


start_link() ->
    gen_server:start_link({local,rooms},?MODULE,undefined,[]).

init(undefined) ->
    {ok,[]}.

send_message(Username,Message) ->
    lager:info("Got a message~n"),
    gen_server:cast(?MODULE, {send,{Username,Message,erlang:localtime()}}).

messages() ->
    gen_server:call(?MODULE,{get}).

handle_cast({send,Message},State) ->
    {noreply,[Message | State]}.

handle_call({get},_From,State) ->
    {reply,State,State}.

get_messages() ->
    get_last_messages(erlang:length(messages())).

get_last_messages(Num) ->
    F = fun({Name,Msg,Time}) ->
        {{_,_,_},{H,M,_}} = Time,
        io:format("~p:~p  ~p: ~p~n",[H,M,Name,Msg])
    end,
    lists:foreach(F,lists:sublist(messages(),Num)).
