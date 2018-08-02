-module(chat_room).
-author("Kehitt").

%% API
-export([new_message/2, get_messages/0, format_messages/1]).

-type chat_messages() :: [chat_message()].
-type chat_message() :: {string(), calendar:time(), string()}.

%% gen_server
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

-type state() :: #{
    messages => chat_messages()
}.

%%
%% API
%%

-spec new_message(string(), string()) ->
    ok.
new_message(Username, Msg) ->
    gen_server:cast(chat_room, {new_message, {Username, erlang:time(), Msg}}).

-spec get_messages() ->
    chat_messages().
get_messages() ->
    gen_server:call(chat_room, get_messages).

-spec format_messages(integer()) ->
    no_return().
format_messages(Num) ->
    lists:foreach(
        fun({Name, Time, Message}) ->
            io:format("~p ~p ~p~n", [Time, Name, Message])
        end,
        lists:sublist(get_messages(), Num)
    ).

%%
%% gen_server
%%

-spec start_link() ->
    {ok, pid()} | {error, _}.
start_link() ->
    gen_server:start_link({local, chat_room}, ?MODULE, undefined, []).

-spec init(atom()) ->
    {ok, state()}.
init(undefined) ->
    State = #{messages => []},
    lager:info("New chat room has been created~n"),
    {ok, State}.

handle_call(get_messages, _, State = #{messages := Messages}) ->
    {reply, Messages, State}.

handle_cast({new_message, NewMsg}, State = #{messages := Messages}) ->
    lager:info("New message ~p", [NewMsg]),
    {noreply, State#{messages := [NewMsg | Messages]}}.
