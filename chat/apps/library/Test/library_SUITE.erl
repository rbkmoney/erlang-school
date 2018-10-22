-module(library_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tests descriptions

-export([all           /0]).
-export([init_per_suite/1]).
-export([end_per_suite /1]).

%% tests

-export([symmetry   /1]).
-export([encode_json/1]).
-export([decode_json/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ENCODED_MSG, <<"{\"user\":\"Igor\",\"text\":\"\",\"room\":\"rm1\",\"event\":\"leave\"}">>).
-define(DECODED_MSG,                                               {leave, <<"Igor">>, <<"rm1">>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [atom()].

all() ->
    [
        encode_json,
        decode_json,
        symmetry
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(library),
    [{apps, [Apps]} | C].

-spec end_per_suite(C :: config()) ->
    term().

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps, C)].

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode_json(C :: config()) ->
    term().

encode_json(_C) ->
    ?ENCODED_MSG = library_protocol:encode(?DECODED_MSG).

-spec decode_json(C :: config()) ->
    term().

decode_json(_C) ->
    ?DECODED_MSG = library_protocol:decode(?ENCODED_MSG).

-spec symmetry(C :: config()) ->
    term().

symmetry(_C) ->
    ?DECODED_MSG = library_protocol:decode(library_protocol:encode(?DECODED_MSG)).
