-module(chatserv_room).

%% API
-define(DEFAULT_DISPLAY_NAME, "New User").
-define(MESSAGE_SENDOUT_TIMEOUT, 1000).

-type state() :: #{
    members := [room_user()],
    messages := [room_message()],
    id := non_neg_integer(), %@todo maybe unneeded
    name := nonempty_string()
}.

-type room_user() :: #{
    display_name := nonempty_string(),
    socket_pid := pid()
}.

-type room_message() :: {DateTime :: erlang:timestamp(), Name :: nonempty_string(), Message :: nonempty_string()}.

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
%% gen_server
%%

-spec start_link(non_neg_integer(), nonempty_string()) ->
    {ok, pid()} | {error, _}.
start_link(Id, Name) ->
    gen_server:start_link(?MODULE, [Id, Name], []).

-spec init(list()) ->
    {ok, state()}.
init([Id, Name]) ->
    erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    {ok, #{members => [], messages => [], id => Id, name => Name}}.

-spec handle_call(any(), any(), state()) ->
    {noreply, state()}.
handle_call(_, _, State) ->
    {noreply, State}.

-spec handle_cast(tuple(), state()) ->
    {noreply, state()}.

handle_cast({join_room, Pid}, State = #{members := Members, id := Id, name := Name}) ->
    case get_member_by_pid(Pid, Members) of
        false ->
            NewMember = #{display_name => ?DEFAULT_DISPLAY_NAME, socket_pid => Pid},
            NewMemberList = [NewMember | Members],
            ok = lager:info(
                "New user joined room (~p, ~p): ~p. Current member list: ~p",
                [Id, Name, NewMember, NewMemberList]
            ),

            {noreply, State#{members := NewMemberList}};
        _ ->
            {noreply, State}
    end;

handle_cast({leave_room, Pid}, State = #{members := Members, id := Id, name := Name}) ->
    case get_member_by_pid(Pid, Members) of
        false ->
            {noreply, State};

        Member ->
            NewMemberList = lists:delete(Member, Members),
            ok = lager:info(
                "User left room (~p, ~p): ~p. Current member list: ~p",
                [Id, Name, Member, NewMemberList]
            ),

            {noreply, State#{members := NewMemberList}}
    end;

handle_cast({set_name, Pid, NewName}, State = #{members := Members}) ->
    case get_member_by_pid(Pid, Members) of
        false ->
            {noreply, State};

        Member ->
            NewMember = Member#{display_name => NewName},
            NewMemberList = replace(Member, NewMember, Members),
            ok = lager:info(
                "A member has changed their name. Old: ~p; New: ~p; New list: ~p",
                [Member, NewMember, NewMemberList]
            ),

            {noreply, State#{members := NewMemberList}}
    end;

handle_cast({send_message, Pid, NewMessageText}, State) ->
    #{id := Id, name := Name, members:= Members, messages := Messages} = State,
    #{display_name := MemberName} = get_member_by_pid(Pid, Members),

    NewMessage = {erlang:universaltime(), MemberName, NewMessageText},
    NewMessages = [NewMessage | Messages],
    ok = lager:info("New message in room (~p,~p): ~p", [Id, Name, NewMessage]),

    {noreply, State#{messages := NewMessages}}.

%%send_messages
-spec handle_info(send_messages, state()) ->
    {noreply, state()}.

handle_info(send_messages, State = #{id:= RoomId, members:= Members, messages := Messages}) when length(Messages) > 0 ->
    lists:foreach(
        fun(Mem) ->
            Pid = maps:get(socket_pid, Mem),
            ok = lager:info("Sending new messages to ~p", [Pid]),
            gen_server:cast(Pid, {tcp_send, {receive_messages, RoomId, Messages}})
        end,
        Members
    ),
    ok = set_sendout_timeout(),
    {noreply, State#{messages:= []}};

handle_info(send_messages, State) ->
    ok = set_sendout_timeout(),
    {noreply, State}.

%%
%% Internal
%%

-spec set_sendout_timeout() ->
    ok.
set_sendout_timeout() ->
    erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    ok.

-spec get_member_by_pid(pid(), [room_user()]) ->
    room_user() | false.
get_member_by_pid(Pid, Members) ->
    Results = lists:filter(
        fun(M) ->
            maps:get(socket_pid, M) == Pid
        end,
        Members
    ),
    case Results of
        [Result] -> Result;
        _ -> false
    end.


-spec replace(term(), term(), list()) ->
    list().
replace(Old, New, List) -> replace(Old, New, List, []).

replace(_, _, [], Acc) -> lists:reverse(Acc);
replace(Old, New, [Old | List], Acc) -> replace(Old, New, List, [New | Acc]);
replace(Old, New, [Other | List], Acc) -> replace(Old, New, List, [Other | Acc]).