-module(chat_server_sup).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(supervisor).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type supervisor_args() :: supervisor:sup_flags().
-type child_args() :: supervisor:child_spec().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    {ok, _} = supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, {supervisor_args(), [child_args()]}}.

init(undefined) ->
    ok = lager:notice("Chat_server supervisor Initialized"),
    {ok, Host} = application:get_env(chat_server, host),
    {ok, Port} = application:get_env(chat_server, port),
    ok = lager:debug("Application will run on ~p:~p", [Host, Port]),
    Dispatch = cowboy_router:compile([
    {Host, [
            {"/", cowboy_static, {priv_file, chat_server, "index.html"}},
            {"/websocket", chat_server_ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_server, "static"}}
        ]}
    ]),
    Connection = ranch:child_spec(http, ranch_tcp, [{port, Port}], cowboy_clear, #{env => #{dispatch => Dispatch}}),
    ok = lager:notice("Launched cowboy on ~p:~p", [Host, Port]),
    RoomManager = #{
        id => manager,
        type => worker,
        start => {chat_server_room_manager, start_link, []}
    },
    {ok, {#{}, [RoomManager, Connection]}}.
