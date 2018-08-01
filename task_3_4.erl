-module(task_3_4).
-import(task_3_3,[new/0,lookup/1,insert/2]).
-export([test/1,worker/0]).

test(N) ->
    process_flag(trap_exit,true),
    task_3_3:new(),
    PIDs = [spawn_link(task_3_4,worker,[]) || _ <- lists:seq(1,N)],
    timer:sleep(1000),
    lists:map(fun(PID) -> exit(PID,stop) end,PIDs),%Самый простой способ завершить все эти процессы
    ets:delete(database),
    over.

generate_key(Length) ->
    [97+rand:uniform(25)|| _ <- lists:seq(1,Length)]. %Рандомный код строчной латинской буквы

worker() ->
    Key = generate_key(3),
    Value = generate_key(4),
    task_3_3:insert(Key,Value),
    io:format("Inserted {~p,~p}~n",[Key,Value]),
    Value = task_3_3:lookup(Key),
    io:format("Received {~p,~p}~n",[Key,Value]),
    timer:sleep(10),
    worker().
