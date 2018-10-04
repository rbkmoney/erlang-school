-module(chat_server_sup).
-include_lib("kernel/include/inet.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(supervisor).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DEFAULT_IP, {127, 0, 0, 1}).

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
    case inet:gethostbyname(Host) of
        {ok, HostMap} ->
            [Ip] = HostMap#hostent.h_addr_list;
        {error, _} ->
            Ip = ?DEFAULT_IP
    end,
    ok = lager:debug("Application will run on ~p:~p", [Ip, Port]),
    Dispatch = cowboy_router:compile([
    {'_', [
            {"/", cowboy_static, {priv_file, chat_server, "index.html"}},
            {"/websocket", chat_server_ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_server, "static"}}
        ]}
    ]),
    Connection = ranch:child_spec(http, ranch_tcp, [{port, Port}, {ip, Ip}], cowboy_clear, #{env => #{dispatch => Dispatch}}),
    ok = lager:notice("Launched cowboy on ~p:~p", [Host, Port]),
    RoomManager = #{
        id => manager,
        type => worker,
        start => {chat_server_room_manager, start_link, []}
    },
    {ok, {#{}, [RoomManager, Connection]}}.
