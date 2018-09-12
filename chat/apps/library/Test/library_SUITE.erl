-module(library_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [atom()].

all() ->
    [
        encodeJson,
        decodeJson,
        symmetry
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DECODED_MSG, {left, <<"Igor">>, <<"Hello">>, <<"rm1">>}).
-define(ENCODED_MSG, <<"{\"user\":\"Igor\",\"room\":\"rm1\",\"message\":\"Hello\",\"event\":\"left\"}">>).

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(library),
    [{apps, [Apps]}|C].

-spec end_per_suite(C :: config()) ->
    term().

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps, C)].

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec encodeJson(C :: config()) ->
    term().

encodeJson(_C) ->
    ?ENCODED_MSG = protocol:encode(?DECODED_MSG).

-spec decodeJson(C :: config()) ->
    term().

decodeJson(_C) ->
    ?DECODED_MSG = protocol:decode(?ENCODED_MSG).

-spec symmetry(C :: config()) ->
    term().

symmetry(_C) ->
    ?DECODED_MSG = protocol:decode(protocol:encode(?DECODED_MSG)).
