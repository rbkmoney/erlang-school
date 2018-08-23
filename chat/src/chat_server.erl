-module(chat_server).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([start_link/0]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).

%%
%% API exports
%%

-export([send/2]).
-export([register_connection/2]).
-export([stop/0]).

%%
%% Type exports
%%

-export_type([username/0]).
-export_type([message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message() :: binary() | string().
-type username() :: message().
-type state() :: map().
-type websocket_down_message() :: {'DOWN', reference(), process, pid(), term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send(message(), pid()) ->
    ok.
send(Message, Source) ->
    gen_server:cast(?MODULE, {send, {Source, Message}}),
    ok.

-spec register_connection(username(), pid()) ->
    ok.
register_connection(Username, PID) ->
    gen_server:call(?MODULE, {register, {Username, PID}}),
    ok.

-spec stop() ->
    stopped.
stop() ->
    gen_server:cast(?MODULE, stop),
    stopped.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%
-spec broadcast(Message :: message(), State :: map()) ->
    ok.
broadcast(Message, State) ->
    lager:info("Sending message to all users"),
    RecipientList = maps:keys(State),
    [inform(Message, Recipient) || Recipient <- RecipientList],
    ok.

-spec inform(message(), pid()) ->
    ok.
inform(Message, Recipient) ->
    lager:info("Sending erlang message to process ~p",[Recipient]),
    Recipient ! {send, Message},
    ok.

-spec register_user(Username :: username(), PID :: pid(), State :: state()) ->
    state().
register_user(Username, PID, State) ->
    lager:info("Registration of new user ~p", [Username]),
    NewState = maps:put(PID, Username, State),
    erlang:monitor(process, PID),
    broadcast(<<Username/binary, " joined this chat">>, NewState),
    NewState.

-spec get_user(pid(), state()) ->
    username().
 get_user(PID, State) ->
     maps:get(PID, State, "Incognito").

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, chat_server}, ?MODULE, undefined, []).

-spec init(undefined) ->
    {ok, state()}.
init(undefined) ->
    lager:notice("Initialized chat room"),
    {ok, #{}}.

-spec handle_cast
    ({send, {pid(), message()}}, state()) ->
        {noreply, state()};
    (stop, state()) ->
        {stop, normal, state()}.
handle_cast({send, {Source, Message}}, State) ->
    Username = get_user(Source, State),
    lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    broadcast(<<Username/binary, ": ", Message/binary>>, State),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_call(term(), term(), state()) ->
    {reply, ok, state()}.
handle_call({register, {Username, PID}}, _From, State) ->
    NewState = register_user(Username, PID, State),
    {reply, ok, NewState};

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_info(websocket_down_message(), state()) ->
    {noreply, state()}.
handle_info({'DOWN', _, process, PID, _}, State) ->
    User = get_user(PID, State),
    lager:info("User ~p disconnected", [User]),
    NewState = maps:remove(PID, State),
    Message = <<User/binary, " left this chat">>,
    broadcast(Message, NewState),
    {noreply, NewState}.

-spec terminate(normal, state()) ->
    ok.
terminate(normal, _State) ->
    ok.
