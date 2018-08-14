-module(chatserv_room).

%% API
-define(DEFAULT_DISPLAY_NAME, "New User").
-define(MESSAGE_SENDOUT_TIMEOUT, 1000).

-type room_state() :: #{
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

-spec init([any(), ...]) ->
    {ok, room_state()}.
init([Id, Name]) ->
    erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    {ok, #{members => [], messages => [], id => Id, name => Name}}.

-spec handle_call(any(), any(), room_state()) ->
    {noreply, room_state()}.
handle_call(_, _, State) ->
    {noreply, State}.

-spec handle_cast(tuple(), room_state()) ->
    {noreply, room_state()}.
handle_cast({join_room, Pid}, State = #{members := Members, id := Id, name := Name}) ->
    %@todo prevent the same pid from joining the same room multiple times
    NewMember = #{display_name => ?DEFAULT_DISPLAY_NAME, socket_pid => Pid},
    NewMemberList = [NewMember | Members],
    lager:info("New user joined room (~p, ~p): ~p. Current member list: ~p", [Id, Name, NewMember, NewMemberList]),
    {noreply, State#{members := NewMemberList}};

handle_cast({set_name, Pid, NewName}, State = #{members := Members}) ->
    %@todo this needs refactoring
    %wtf
    [Member] = lists:filter(fun(M) -> maps:get(socket_pid, M) == Pid end, Members),
    NewMember = Member#{display_name => NewName},
    NewMembers = replace(Member, NewMember, Members),
    lager:info("A member has changed their name. Old: ~p; New: ~p; New list: ~p", [Member, NewMember, NewMembers]),
    %
    {noreply, State#{members := NewMembers}};

handle_cast({send_message, Pid, NewMessageText},
    State = #{id := Id, name := Name, members:= Members, messages := Messages}) ->
    %@todo this needs refactoring
    %no
    [_Member = #{display_name := MemberName}] = lists:filter(fun(M) -> maps:get(socket_pid, M) == Pid end, Members),
    NewMessage = {erlang:universaltime(), MemberName, NewMessageText},
    NewMessages = [NewMessage|Messages],
    %
    lager:info("New message in room (~p,~p): ~p", [Id, Name, NewMessage]),
    {noreply, State#{messages := NewMessages}}.

%%send_messages
-spec handle_info(send_messages, room_state()) ->
    {noreply, room_state()}.
handle_info(send_messages, State = #{id:= RoomId, members:= Members, messages := Messages}) when length(Messages) > 0 ->
    lists:foreach(
        fun(Mem) ->
            Pid = maps:get(socket_pid, Mem),
            MessagesBin = term_to_binary(Messages),
            lager:info("Sending new messages to ~p: ~p", [Pid, MessagesBin]),
            gen_server:cast(Pid, {server_reply, <<0, 5, RoomId:16/unsigned, MessagesBin/binary>>})
        end,
        Members
    ),
    erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    {noreply, State#{messages:= []}};

handle_info(send_messages, State) ->
    erlang:send_after(?MESSAGE_SENDOUT_TIMEOUT, self(), send_messages),
    {noreply, State}.

%%
%% Internal
%%

-spec replace(term(), term(), list()) ->
    list().
replace(Old, New, List) -> replace(Old, New, List, []).

replace(_, _, [], Acc) -> lists:reverse(Acc);
replace(Old, New, [Old | List], Acc) -> replace(Old, New, List, [New | Acc]);
replace(Old, New, [Other | List], Acc) -> replace(Old, New, List, [Other | Acc]).