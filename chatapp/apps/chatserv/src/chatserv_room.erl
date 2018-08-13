-module(chatserv_room).

%% API
-type room_state() :: #{
    members := [pid()],
    id := non_neg_integer(),
    name := nonempty_string()
}.

%% gen_server
-behavior(gen_server).
-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2
]).

%%
%% gen_server
%%

-spec start_link(non_neg_integer(), nonempty_string()) ->
    {ok, pid()} | {error, _}.
start_link(Id, Name) ->
    gen_server:start_link(?MODULE, [Id, Name], []).

-spec init([]) ->
    {ok, room_state()}.
init([Id, Name]) ->
    {ok, #{members => [], id => Id, name => Name}}.

-spec handle_call(any(), any(), room_state()) ->
    {noreply, room_state()}.
handle_call(_, _ , State) ->
    {noreply, State}.

-spec handle_cast(tuple(), room_state()) ->
    {noreply, room_state()}.
handle_cast(_, State) ->
    {noreply, State}.