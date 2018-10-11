-module(chat_server_sup).

-include_lib("kernel/include/inet.hrl").

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
    Dispatch = cowboy_router:compile([
        {'_',
            [
                {"/", cowboy_static, {priv_file, chat_server, "index.html"}},
                {"/websocket", chat_server_ws_handler, []},
                {"/static/[...]", cowboy_static, {priv_dir, chat_server, "static"}}
            ]
        }
    ]),
    {Host, Port} = get_host_and_port(),
    Ip = get_ip(Host),
    ok = lager:debug("Application will run on ~p, port ~p", [Ip, Port]),
    Connection = ranch:child_spec(
        http,
        ranch_tcp,
        [
            {port, Port},
            {ip, Ip}
        ],
        cowboy_clear,
        #{env => #{dispatch => Dispatch}}
    ),
    ok = lager:notice("Cowboy will be launched on ~p:~p", [Host, Port]),
    RoomManager = #{
        id => manager,
        type => worker,
        start => {chat_server_room_manager, start_link, []}
    },
    {ok, {#{}, [RoomManager, Connection]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

get_host_and_port() ->
    Host = application:get_env(chat_server, host, "localhost"),
    Port = application:get_env(chat_server, port, 8080),
    ok = lager:debug("Host received: ~p; Port received ~p", [Host, Port]),
    {Host, Port}.

get_ip(Host) ->
    {ok, HostRec} = inet:gethostbyname(Host),
    IpList = HostRec#hostent.h_addr_list,
    [Ip | _] = IpList, % Assume that needed IP is first, idk how we should behave with multiple IPs
    Ip.
