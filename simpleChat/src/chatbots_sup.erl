-module(chatbots_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

-define(SERVER,?MODULE).

-spec start_link() ->
    {ok,pid()}.
start_link() ->
    supervisor:start_link({local,?SERVER}, ?MODULE,[]).

init([]) ->
    lager:info("Bot supervisor initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 1,
        period => 1
    },
    ChatBots = [
        #{id => bot_1, start => {chatbots, start_link, ['Adam']}},
        #{id => bot_2, start => {chatbots, start_link, ['Betty']}},
        #{id => bot_3, start => {chatbots, start_link, ['Charlie']}}
    ],
    {ok,{SupArgs,ChatBots}}.
