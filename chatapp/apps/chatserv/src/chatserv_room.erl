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

-type member_id() :: chatlib_proto:auth_id().
-type member_map() :: #{member_id() => room_member()}.

-type room_member() :: #{
    display_name := chatlib_proto:member_name()
}.

-export([
    join/2,
    change_member_name/3,
    send_message/3,
    get_room_name/1,
    via_roomid/1
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

-spec join(chatlib_proto:auth_id(), chatlib_proto:room_id_direct()) ->
    chatlib_proto:response_code().
join(MemberId, RoomId) ->
    gen_server:call(via_roomid(RoomId), {join_room, MemberId}).

-spec change_member_name(chatlib_proto:auth_id(), chatlib_proto:room_id_direct(), chatlib_proto:member_name()) ->
    chatlib_proto:response_code().
change_member_name(MemberId, RoomId, Name) ->
    gen_server:call(via_roomid(RoomId), {set_name, MemberId, Name}).

-spec send_message(chatlib_proto:auth_id(), chatlib_proto:room_id_direct(), chatlib_proto:message_text()) ->
    chatlib_proto:response_code().
send_message(MemberId, RoomId, MessageText) ->
    gen_server:call(via_roomid(RoomId), {send_message, MemberId, MessageText}).

-spec get_room_name(chatlib_proto:room_id_direct()) ->
    chatlib_proto:room_name().
get_room_name(RoomId) ->
    gen_server:call(via_roomid(RoomId), get_room_name).

-spec via_roomid(chatlib_proto:room_id_direct()) ->
    {via, gproc, {n, l, {chat_room, chatlib_proto:room_id_direct()}}}.
via_roomid(RoomId) ->
    {via, gproc, {n, l, {chat_room, RoomId}}}.

%%
%% gen_server
%%

-spec start_link(non_neg_integer(), nonempty_string()) ->
    {ok, pid()} | {error, _}.
start_link(Id, Name) ->
    gen_server:start_link(via_roomid(Id), ?MODULE, [Id, Name], []).

-spec init(list()) ->
    {ok, state()}.
init([Id, Name]) ->
    _ = set_sendout_timeout(),

    {ok, #{members => #{}, pending_messages => [], id => Id, name => Name}}.

-spec handle_call(
    get_room_name |
    {join_room, member_id()} |
    {set_name, member_id(), chatlib_proto:member_name()} |
    {send_message, member_id(), chatlib_proto:message_text()},
    any(), state()
) ->
    {reply, ok | badarg | chatlib_proto:room_name(), state()}.

handle_call(get_room_name, _, State = #{name := Name}) ->
    {reply, Name, State};

handle_call({join_room, MemberId}, _, State = #{members := Members, id := Id, name := Name}) ->
    %@todo redundant check here, possibly get rid of
    case add_new_member(MemberId, ?DEFAULT_DISPLAY_NAME, Members) of
        {ok, NewMemberList} ->
            ok = lager:info(
                "New user joined room (~p, ~p). Current member list: ~p",
                [Id, Name, NewMemberList]
            ),

            _ = monitor_member(MemberId),

            {reply, ok, State#{members := NewMemberList}};

        {error, already_exists} ->
            {reply, user_already_exists, State}
    end;

handle_call({set_name, MemberId, NewName}, _, State = #{members := Members}) ->
    NewMemberList = do_change_member_name(MemberId, NewName, Members),

    ok = lager:info(
        "A member has changed their name. New: ~p; New list: ~p",
        [NewName, NewMemberList]
    ),

    {reply, ok, State#{members := NewMemberList}};

handle_call({send_message, MemberId, NewMessageText}, _, State) ->
    #{id := Id, name := Name, members:= Members, pending_messages := Messages} = State,

    #{display_name := MemberName} = get_member(MemberId, Members),
    NewMessages = add_new_message(MemberName, NewMessageText, Messages),

    ok = lager:info("New message in room (~p,~p): ~p", [Id, Name, NewMessages]),

    {reply, ok, State#{pending_messages := NewMessages}};

handle_call({leave_room, MemberId}, _, State = #{members := Members, id := Id, name := Name}) ->
    NewMemberList = remove_member(MemberId, Members),

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
-spec handle_info(send_messages | {gproc, unreg, _, {n, l, {chat_member, member_id()}}}, state()) ->
    {noreply, state()}.

handle_info(send_messages, State = #{id:= RoomId, members:= Members, pending_messages := Messages}) ->
    case length(Messages) > 0 of
        true ->
            _ = maps:fold(
                fun(MemberId, _, ok) ->
                    ok = chatserv_wshandler:send_messages(MemberId, RoomId, Messages)
                end,
                ok, Members
            );

        false ->
            false %@todo this is even worse
    end,

    ok = set_sendout_timeout(),
    {noreply, State#{pending_messages:= []}};

handle_info({gproc, unreg, _, {n, l, {chat_member, MemberId}}}, State) ->
    #{members := Members, id := Id, name := Name} = State,
    NewMemberList = remove_member(MemberId, Members),

    ok = lager:info(
        "User disconnected from room (~p, ~p); Current member list: ~p",
        [Id, Name, NewMemberList]
    ),

    {noreply, State#{members := NewMemberList}}.

%%
%% Internal
%%
-spec monitor_member(member_id()) ->
    reference().
monitor_member(MemberId) ->
    gproc:monitor({n, l, {chat_member, MemberId}}).

-spec member_exists(member_id(), Current :: member_map()) ->
    Result :: boolean().
member_exists(MemberId, Members) ->
    maps:is_key(MemberId, Members).

-spec add_new_member(member_id(), chatlib_proto:member_name(), Old :: member_map()) ->
    {ok, New :: member_map()} | {error, already_exists}.
add_new_member(MemberId, MemberName, Members) ->
    case member_exists(MemberId, Members) of
        false ->
            NewMember = #{display_name => MemberName},
            {ok, maps:put(MemberId, NewMember, Members)};
        _ ->
            {error, already_exists}
    end.

-spec do_change_member_name(member_id(), chatlib_proto:member_name(), Old :: member_map()) ->
    New :: member_map().
do_change_member_name(MemberId, NewName, Members) ->
    Member = get_member(MemberId, Members),
    NewMember = Member#{display_name => NewName},

    maps:put(MemberId, NewMember, Members).

-spec get_member(member_id(), member_map()) ->
    room_member().
get_member(MemberId, Members) ->
    maps:get(MemberId, Members).

-spec remove_member(member_id(), Old :: member_map()) ->
    New :: member_map().
remove_member(MemberId, Members) ->
    maps:remove(MemberId, Members).

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
