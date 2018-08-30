-module(chatserv_room).

%% API
-define(DEFAULT_DISPLAY_NAME, "New User").
-define(MESSAGE_SNDOUT_INTERVAL, 1000).

-type state() :: #{
    members := member_map(),
    pending_messages := chatlib_proto:message_list(),
    id := chatlib_proto:room_id_direct(),
    name := chatlib_proto:room_name()
}.

-type member_pid() :: pid().
-type member_map() :: #{member_pid() => room_member()}.

-type room_pid() :: pid().
-type room_member() :: #{
    display_name := chatlib_proto:member_name()
}.

-export([
    join/1,
    change_member_name/2,
    send_message/2,
    get_room_name/1
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

-spec join(room_pid()) ->
    chatlib_proto:response_code().
join(Pid) ->
    gen_server:call(Pid, {join_room, self()}).

-spec change_member_name(room_pid(), chatlib_proto:member_name()) ->
    chatlib_proto:response_code().
change_member_name(Pid, Name) ->
    gen_server:call(Pid, {set_name, self(), Name}).

-spec send_message(room_pid(), chatlib_proto:message_text()) ->
    chatlib_proto:response_code().
send_message(Pid, MessageText) ->
    gen_server:call(Pid, {send_message, self(), MessageText}).

-spec get_room_name(room_pid()) ->
    chatlib_proto:room_name().
get_room_name(Pid) ->
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
    _ = set_sendout_timeout(),

    {ok, #{members => #{}, pending_messages => [], id => Id, name => Name}}.

-spec handle_call(
    get_room_name |
    {join_room, member_pid()} |
    {set_name, member_pid(), chatlib_proto:member_name()} |
    {send_message, member_pid(), chatlib_proto:message_text()},
    any(), state()
) ->
    {reply, ok | badarg | chatlib_proto:room_name(), state()}.

handle_call(get_room_name, _, State = #{name := Name}) ->
    {reply, Name, State};

handle_call({join_room, Pid}, _, State = #{members := Members, id := Id, name := Name}) ->
    %@todo redundant check here, possibly get rid of
    case add_new_member(Pid, ?DEFAULT_DISPLAY_NAME, Members) of
        {ok, NewMemberList} ->
            ok = lager:info(
                "New user joined room (~p, ~p). Current member list: ~p",
                [Id, Name, NewMemberList]
            ),

            _ = erlang:monitor(process, Pid),

            {reply, ok, State#{members := NewMemberList}};

        {error, already_exists} ->
            {reply, user_already_exists, State}
    end;

handle_call({set_name, Pid, NewName}, _, State = #{members := Members}) ->
    NewMemberList = change_member_name(Pid, NewName, Members),

    ok = lager:info(
        "A member has changed their name. New: ~p; New list: ~p",
        [NewName, NewMemberList]
    ),

    {reply, ok, State#{members := NewMemberList}};

handle_call({send_message, Pid, NewMessageText}, _, State) ->
    #{id := Id, name := Name, members:= Members, pending_messages := Messages} = State,

    #{display_name := MemberName} = get_member(Pid, Members),
    NewMessages = add_new_message(MemberName, NewMessageText, Messages),

    ok = lager:info("New message in room (~p,~p): ~p", [Id, Name, NewMessages]),

    {reply, ok, State#{pending_messages := NewMessages}};

handle_call({leave_room, Pid}, _, State = #{members := Members, id := Id, name := Name}) ->
    NewMemberList = remove_member(Pid, Members),

    ok = lager:info(
        "User left room (~p, ~p); Current member list: ~p",
        [Id, Name, NewMemberList]
    ),

    {noreply, State#{members := NewMemberList}}.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

%%send_messages
-spec handle_info(send_messages | {'DOWN', reference(), process, pid(), atom()}, state()) ->
    {noreply, state()}.

handle_info(send_messages, State = #{id:= RoomId, members:= Members, pending_messages := Messages}) ->
    case length(Messages) > 0 of
        true ->
            _ = maps:fold(
                fun(Pid, _, ok) ->
                    ok = lager:info("Sending new messages to ~p", [Pid]),
                    ok = chatserv_wshandler:send_messages(Pid, RoomId, Messages)
                end,
                ok, Members
            );

        false ->
            false %@todo this is even worse
    end,

    ok = set_sendout_timeout(),
    {noreply, State#{pending_messages:= []}};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State = #{members := Members, id := Id, name := Name}) ->
    NewMemberList = remove_member(Pid, Members),

    ok = lager:info(
        "User disconnected from room (~p, ~p) with reason ~p; Current member list: ~p",
        [Id, Name, Reason, NewMemberList]
    ),

    {noreply, State#{members := NewMemberList}}.

%%
%% Internal
%%
-spec member_exists(member_pid(), Current :: member_map()) ->
    Result :: boolean().
member_exists(Pid, Members) ->
    maps:is_key(Pid, Members).

-spec add_new_member(member_pid(), chatlib_proto:member_name(), Old :: member_map()) ->
    {ok, New :: member_map()} | {error, already_exists}.
add_new_member(Pid, MemberName, Members) ->
    case member_exists(Pid, Members) of
        false ->
            NewMember = #{display_name => MemberName},
            {ok, maps:put(Pid, NewMember, Members)};
        _ ->
            {error, already_exists}
    end.

-spec change_member_name(member_pid(), chatlib_proto:member_name(), Old :: member_map()) ->
    New :: member_map().
change_member_name(Pid, NewName, Members) ->
    Member = get_member(Pid, Members),
    NewMember = Member#{display_name => NewName},

    maps:put(Pid, NewMember, Members).

-spec get_member(member_pid(), member_map()) ->
    room_member().
get_member(Pid, Members) ->
    maps:get(Pid, Members).

-spec remove_member(member_pid(), Old :: member_map()) ->
    New :: member_map().
remove_member(Pid, Members) ->
    maps:remove(Pid, Members).

-spec add_new_message(chatlib_proto:member_name(), chatlib_proto:message_text(), Old :: chatlib_proto:message_list()) ->
    New :: chatlib_proto:message_list().
add_new_message(MemberName, NewMessageText, Messages) ->
    NewMessage = {erlang:universaltime(), MemberName, NewMessageText},

    [NewMessage | Messages].

-spec set_sendout_timeout() ->
    ok.
set_sendout_timeout() ->
    _ = erlang:send_after(?MESSAGE_SNDOUT_INTERVAL, self(), send_messages),
    ok.
