-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    lager:notice("toppage_handler init"),
	{ok, Req, undefined}.

handle(Req, State) ->
    lager:info("Request: ~p~n", [Req]),
	{ok, Req2} = cowboy_req:chunked_reply(200, Req),
	ok = cowboy_req:chunk("Hello\r\n", Req2),
	ok = timer:sleep(1000),
	ok = cowboy_req:chunk("World\r\n", Req2),
	ok = timer:sleep(1000),
	ok = cowboy_req:chunk("Chunked!\r\n", Req2),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
