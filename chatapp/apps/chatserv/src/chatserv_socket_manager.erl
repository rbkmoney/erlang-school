-module(chatserv_socket_manager).
-behavior(gen_server).

%%API
-type socket_manager_state() :: #{
    clients := [client_socket_info()],
    free_socks := non_neg_integer()
}.

-type client_socket_info() :: {pid(), non_neg_integer() | false}.


%% gen_server
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_continue/2]).


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
        free_socks => 0
    },{
        continue, spawn_acceptors
    }}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({client_connected, _Pid}, State) ->
    {noreply, State};
handle_cast({client_data, _Pid, _Data}, State) ->
    {noreply, State}.

-spec handle_continue(spawn_acceptors, socket_manager_state()) ->
    {noreply, socket_manager_state()}.
handle_continue(spawn_acceptors, State) ->
    {ok, LSocket} = gen_tcp:listen(8888, [{active, once},{packet,2}]),
    NewSockets = lists:map(fun(_) ->
            {ok, Pid} = supervisor:start_child(chsv_socket_sup, [LSocket]),
            erlang:monitor(process, Pid),
            {Pid, false}
        end,
        lists:seq(0, 4)
    ),
    {noreply, State#{clients:= NewSockets, free_socks := length(NewSockets)}}.

%%
%% Internal
%%