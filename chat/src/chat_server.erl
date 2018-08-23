-module(chat_server).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-export([send/2, register_connection/2, stop/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Message, Source) ->
    gen_server:cast(?MODULE, {send, {Source, Message}}).

register_connection(Username, PID) ->
    gen_server:call(?MODULE, {register, {Username, PID}}).

stop() ->
    gen_server:cast(?MODULE,stop).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

broadcast(Message, State) ->
    lager:info("Sending message to all users"),
    RecipientList = maps:keys(State),
    [inform(Message, Recipient) || Recipient <- RecipientList],
    ok.

inform(Message, Recipient) ->
    lager:info("Sending erlang message to process ~p",[Recipient]),
    Recipient ! {send, Message},
    ok.

register_user(Username, PID, State) ->
    lager:info("Registration of new user ~p", [Username]),
    NewState = maps:put(PID, Username, State),
    erlang:monitor(process, PID),
    broadcast(<<Username/binary, " joined this chat">>, NewState),
    NewState.

 get_user(PID, State) ->
     maps:get(PID, State, "Incognito").

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, chat_server}, ?MODULE, undefined,[]).

init(undefined) ->
    lager:notice("Initialized chat room"),
    {ok, #{}}.

handle_cast({send, {Source, Message}}, State) ->
    Username = get_user(Source, State),
    lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    broadcast(<<Username/binary, ": ", Message/binary>>, State),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({register, {Username, PID}}, _From, State) ->
    NewState = register_user(Username, PID, State),
    {reply, ok, NewState};

handle_call({find, PID}, _From, State) ->
    Username = maps:get(PID, State, "Incognito"),
    {reply, Username, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_info({'DOWN', _, process, PID, _}, State) ->
    User = get_user(PID, State),
    lager:info("User ~p disconnected", [User]),
    NewState = maps:remove(PID, State),
    Message = <<User/binary, " left this chat">>,
    broadcast(Message, NewState),
    {noreply, NewState}.

terminate(normal, _State) ->
    ok.
