-module(chat_server_sup).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(supervisor).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0]).
-export([start_link/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type supervisor_args() :: supervisor:sup_flags().
-type child_args() :: supervisor:child_spec().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    start_link('_', 8080).

-spec start_link(Host :: term(), Port :: non_neg_integer()) ->
    {ok, pid()}.

start_link(Host, Port) ->
    {ok, _} = supervisor:start_link({local, ?MODULE}, ?MODULE, #{host => Host, port => Port}).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(map()) ->
    {ok, {supervisor_args(), [child_args()]}}.

init(#{host := Host, port := Port}) ->
    ok = lager:notice("Chat_server supervisor Initialized"),
    Dispatch = cowboy_router:compile([
    {Host, [
            {"/", cowboy_static, {priv_file, chat_server, "index.html"}},
            {"/websocket", chat_server_ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_server, "static"}}
        ]}
    ]),
    Connection = ranch:child_spec(http, ranch_tcp, [{port, Port}], cowboy_clear, #{env => #{dispatch => Dispatch}}),
    RoomManager = #{
        id => manager,
        type => worker,
        start => {chat_server_room_manager, start_link, []}
    },
    {ok, {#{}, [RoomManager, Connection]}}.
