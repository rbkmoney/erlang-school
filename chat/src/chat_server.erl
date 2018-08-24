-module(chat_server).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([start_link/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([send/1]).
-export([stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([username/0]).
-export_type([message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message() :: binary() | string().
-type username() :: message().
-type state() :: map().
-type websocket_down_message() :: {'DOWN', reference(), process, pid(), term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(ClientMessage) ->
    gen_server:cast(?MODULE, {client_message, ClientMessage}),
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

-spec add_user(PID :: pid(), Username :: username(), State :: state()) ->
    state().

add_user(PID, Username, State) ->
    maps:put(PID, Username, State).

-spec remove_user(PID :: pid(), State :: state()) ->
    state().

remove_user(PID, State) ->
    maps:remove(PID, State).

-spec register_user(Username :: username(), PID :: pid(), State :: state()) ->
    state().

register_user(Username, PID, State) ->
    lager:info("Registration of new user ~p", [Username]),
    NewState = add_user(PID, Username, State),
    erlang:monitor(process, PID),
    Reply = protocol:encode(joined, Username),
    broadcast(Reply, NewState),
    NewState.

-spec get_user(pid(), state()) ->
    username().
 get_user(PID, State) ->
     maps:get(PID, State, "Incognito").

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Id) ->
    gen_server:start_link({local, chat_server}, ?MODULE, Id, []).

-spec init(atom()) ->
    {ok, state()}.

init(Id) ->
    lager:notice("Initialized chat room"),
    room_manager:add_room(Id, self()),
    {ok, #{}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({client_message, {send_message, Message, Source}}, State) ->
    Username = get_user(Source, State),
    lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    Reply = protocol:encode(message, Username, Message),
    broadcast(Reply, State),
    {noreply, State};

handle_cast({client_message, {register, Username, Source}}, State) ->
    NewState = register_user(Username, Source, State), % It actuay works, even if client sends message immediatly after calling for registration, wow!
    {noreply, NewState}.

-spec handle_call(term(), term(), state()) ->
    {reply, ok, state()}.

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_info(websocket_down_message(), state()) ->
    {noreply, state()}.

handle_info({'DOWN', _, process, PID, _}, State) ->
    Username = get_user(PID, State),
    lager:info("User ~p disconnected", [Username]),
    NewState = remove_user(PID, State),
    Reply = protocol:encode(left, Username),
    broadcast(Reply, NewState),
    {noreply, NewState}.

-spec terminate(normal, state()) ->
    ok.
terminate(normal, _State) ->
    ok.
