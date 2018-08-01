-module(chat_room).
-author("Kehitt").

-behavior(gen_server).
%% gen_server
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

%% API
-export([new_message/2, get_messages/0, format_messages/1]).

start_link() ->
    gen_server:start_link({local, chat_room}, ?MODULE, undefined, []).

%%
%% gen_server
%%
init(undefined) ->
    State = #{ messages => []},
    {ok, State}.

handle_call({get_messages}, _, State = #{messages := Messages}) ->
    {reply, Messages, State}.

handle_cast({new_message, NewMsg}, State = #{ messages := Messages }) ->
    {noreply, State#{messages := [NewMsg|Messages]}}.

%%
%% API
%%
new_message(Username, Msg) ->
    gen_server:cast(chat_room, {new_message, {Username, erlang:time(), Msg}}).

get_messages() ->
    gen_server:call(chat_room, {get_messages}).

format_messages(Num) ->
    lists:foreach(
        fun({Name, Time, Message}) ->
            io:format("~p ~p ~p~n", [Time, Name, Message])
        end,
        lists:sublist(get_messages(), Num)
    ).