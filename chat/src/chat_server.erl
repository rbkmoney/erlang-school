-module(chat_server).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3, handle_info/2]).

-export([send/2, register_connection/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Message, Source) ->
    Username = get_user_by_pid(Source),
    gen_server:cast(?MODULE, {send, {Username, Message}}).

register_connection(Username, PID) ->
    gen_server:call(?MODULE, {register, {Username, PID}}),
    send_to_all(<<Username/binary, " joined this chat">>).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

get_user_by_pid(PID) ->
    gen_server:call(?MODULE, {find, PID}).

send_to_all(Message) ->
    gen_server:cast(?MODULE, {send_to_all, Message}).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, chat_server}, ?MODULE, undefined,[]).

init(undefined) ->
    lager:notice("Initialized room2"),
    {ok, #{}}.

handle_cast({send, {Username, Message}}, State) ->
    lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    send_to_all(<<Username/binary, ": ", Message/binary>>),
    {noreply, State};

handle_cast({send_to_all, Message}, State) ->
    lager:info("Sending message to all users"),
    ConnectionsList = maps:keys(State),
    F = fun(PID, Msg) ->
        lager:info("Sending erlang message to process ~p",[PID]),
        PID ! {send_back,Msg}
    end,
    [F(Item, Message) || Item <- ConnectionsList],
    {noreply, State}.

handle_call({register, {Username, PID}}, _From, State) ->
    lager:info("Registration of new user ~p", [Username]),
    NewState = maps:put(PID, Username, State),
    erlang:monitor(process, PID),
    {reply, ok, NewState};

handle_call({find, PID}, _From, State) ->
    Username = maps:get(PID, State, "Incognito"),
    {reply, Username, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_info({'DOWN', _, process, PID, _}, State) ->
    lager:info("User with PID ~p disconnected", [PID]),
    NewState = maps:remove(PID, State),
    {noreply, NewState}.
