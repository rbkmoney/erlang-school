-module(task_3_5).
-export([test/0,get_date/1]).


test() ->
    {Year,Month,Day} = erlang:date(),
    {Year,Month,Day} = query(),
    test_passed.

query() ->
    Server = spawn(task_3_5,get_date,[self()]),
    erlang:monitor(process, Server),
    Server ! time,
    receive
        {date, Date} ->
            Date;
        {'DOWN',_,_,Server,_} ->
            io:format("Failed, retrying~n"),
            query()
    end.

get_date(Parent) ->
    receive
        time ->
            case rand:uniform() > 0.5 of
                true -> Parent ! {date, erlang:date()};
                false -> exit(self(),dunnothedate)
            end
    end.
