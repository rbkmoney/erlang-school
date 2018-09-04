-module(chatserv_sup).

-behaviour(supervisor).

%% API
-type sv_sl_result() :: {ok, Pid :: pid()} | {error, Reason :: term()}.
-type sv_init_result() :: {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.


-export([start_link/0]).
-export_type([sv_sl_result/0, sv_init_result/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, chsv_top_sup).

%%
%% API
%%

-spec start_link() ->
    sv_sl_result().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%
%% Supervisor callbacks
%%

-spec init([]) ->
    sv_init_result().
init([]) ->
    SupFlags = #{},
    Children = [
        #{
            id => room_manager,
            start => {chatserv_room_manager, start_link, []}
        }
    ],
    {ok, {SupFlags, Children}}.

%%
%% Internal functions
%%
