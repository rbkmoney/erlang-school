-module(chatserv_socket_manager).
-behavior(gen_server).

%%API
-type socket_manager_state() :: #{
    clients := [pid()],
    listener := gen_tcp:socket() | undefined
}.

%% gen_server
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2
]).


%%
%% gen_server
%%

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, socket_manager}, ?MODULE, [], []).

init([]) ->
    {ok, #{
        clients => [],
        listener => undefined
    }, {
        continue, spawn_acceptors
    }}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({client_connected, _}, State = #{clients := Clients, listener := LSocket}) ->
    {ok, Pid} = supervisor:start_child(chsv_socket_sup, [LSocket]),
    erlang:monitor(process, Pid),

    {noreply, State#{clients := [Pid | Clients]}}.

-spec handle_continue(spawn_acceptors, socket_manager_state()) ->
    {noreply, socket_manager_state()}.
handle_continue(spawn_acceptors, State) ->
    {ok, ListenIp} = application:get_env(chatserv, listen_ip),
    {ok, ListenPort} = application:get_env(chatserv, listen_port),

    {ok, LSocket} = gen_tcp:listen(ListenPort, [binary, {active, once}, {ip, ListenIp}]),

    NewSockets = lists:map(
        fun(_) ->
            {ok, Pid} = supervisor:start_child(chsv_socket_sup, [LSocket]),
            erlang:monitor(process, Pid),
            Pid
        end,
        lists:seq(0, 4)
    ),

    {noreply, State#{clients:=NewSockets, listener:=LSocket}}.

-spec handle_info({'DOWN', reference(), process, pid(), killed | tcp_closed}, socket_manager_state()) ->
    {noreply, socket_manager_state()}.
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State}.

%%
%% Internal
%%