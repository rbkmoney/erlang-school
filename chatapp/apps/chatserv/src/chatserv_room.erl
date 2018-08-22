-module(chatserv_room).

%% API
-define(DEFAULT_DISPLAY_NAME, "New User").
-define(MESSAGE_SENDOUT_TIMEOUT, 1000).

-type state() :: #{
    members := member_map(),
    messages := [chatlib_proto:member_message()],
    id := chatlib_proto:room_id(),
    name := chatlib_proto:room_name()
}.

-type member_map() :: #{ pid() => room_member() }.

-type room_member() :: #{
    display_name := chatlib_proto:member_name()
}.

-export([
    join_to/2,
    change_name_in/3,
    send_message_to/3,
    get_name_of/1
]).

%% gen_server
-behavior(gen_server).
-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%%
%% API
%%

-spec join_to(pid(), pid()) ->
    ok | badarg.
join_to(Pid, From) ->
    gen_server:call(Pid, {join_room, From}).

-spec change_name_in(pid(), pid(), chatlib_proto:member_name()) ->
    ok | badarg.
change_name_in(Pid, From, Name) ->
    gen_server:call(Pid, {set_name, From, Name}).

-spec send_message_to(pid(), pid(), chatlib_proto:message_text()) ->
    ok.
send_message_to(Pid, From, MessageText) ->
    gen_server:call(Pid, {send_message, From, MessageText}).

-spec get_name_of(pid()) ->
    chatlib_proto:room_name().
get_name_of(Pid) ->
    gen_server:call(Pid, get_room_name).

%%
%% gen_server
%%

-spec start_link(non_neg_integer(), nonempty_string()) ->
    {ok, pid()} | {error, _}.
start_link(Id, Name) ->
    gen_server:start_link(?MODULE, [Id, Name], []).

-spec init(list()) ->
    {ok, state()}.
init([Id, Name]) ->
    _ = erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    {ok, #{members => #{}, messages => [], id => Id, name => Name}}.

-spec handle_call(get_room_name |
                {join_room, pid()} |
                {set_name, pid(), chatlib_proto:member_name()} |
                {send_message, pid(), chatlib_proto:message_text()},
        {pid(), _}, state()
    ) ->
    {reply, ok | badarg | chatlib_proto:room_name(), state()}.

handle_call(get_room_name, _, State = #{name := Name}) ->
    {reply, Name, State};

handle_call({join_room, Pid}, _, State = #{members := Members, id := Id, name := Name}) ->
    case maps:is_key(Pid, Members) of
        false ->
            NewMember = #{display_name => ?DEFAULT_DISPLAY_NAME},
            NewMemberList = maps:put(Pid, NewMember, Members),

            ok = lager:info(
                "New user joined room (~p, ~p): ~p. Current member list: ~p",
                [Id, Name, NewMember, NewMemberList]
            ),

            erlang:monitor(process, Pid),

            {reply, ok, State#{members := NewMemberList}};

        _ ->
            {reply, badarg, State}
    end;

handle_call({set_name, Pid, NewName}, _, State = #{members := Members}) ->
    Member = maps:get(Pid, Members),
    NewMember = Member#{display_name => NewName},
    NewMemberList = maps:put(Pid, NewMember, Members),

    ok = lager:info(
        "A member has changed their name. Old: ~p; New: ~p; New list: ~p",
        [Member, NewMember, NewMemberList]
    ),

    {reply, ok, State#{members := NewMemberList}};

handle_call({send_message, Pid, NewMessageText}, _, State) ->
    #{id := Id, name := Name, members:= Members, messages := Messages} = State,
    #{display_name := MemberName} = maps:get(Pid, Members),

    NewMessage = #{
        timestamp => erlang:universaltime(),
        member_name => MemberName,
        message_text => NewMessageText
    },

    NewMessages = [NewMessage | Messages],

    ok = lager:info("New message in room (~p,~p): ~p", [Id, Name, NewMessage]),

    {reply, ok, State#{messages := NewMessages}}.

-spec handle_cast({leave_room, pid()}, state()) ->
    {noreply, state()}.

%@todo why cant I make this a call?
handle_cast({leave_room, Pid}, State = #{members := Members, id := Id, name := Name}) ->
    NewMemberList = remove_user(Pid, Members),

    ok = lager:info(
        "User left room (~p, ~p); Current member list: ~p",
        [Id, Name, NewMemberList]
    ),

    {noreply, State#{members := NewMemberList}}.

%%send_messages
-spec handle_info(send_messages | {'DOWN', reference(), process, pid(), atom()}, state()) ->
    {noreply, state()}.

handle_info(send_messages, State = #{id:= RoomId, members:= Members, messages := Messages}) when length(Messages) > 0 ->
    _ = maps:fold(
        fun(Pid, _, ok) ->
            ok = lager:info("Sending new messages to ~p", [Pid]),
            ok = chatserv_wshandler:send_messages_to(Pid, RoomId, Messages)
        end,
        ok, Members
    ),
    ok = set_sendout_timeout(),
    {noreply, State#{messages:= []}};

handle_info(send_messages, State) ->
    ok = set_sendout_timeout(),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State = #{members := Members, id := Id, name := Name}) ->
    NewMemberList = remove_user(Pid, Members),

    ok = lager:info(
        "User disconnected from room (~p, ~p) with reason ~p; Current member list: ~p",
        [Id, Name, Reason, NewMemberList]
    ),

    {noreply, State#{members := NewMemberList}}.

%%
%% Internal
%%
-spec remove_user(pid(), member_map()) ->
    member_map().
remove_user(Pid, Members) ->
    maps:remove(Pid, Members).

-spec set_sendout_timeout() ->
    ok.
set_sendout_timeout() ->
    _ = erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    ok.
