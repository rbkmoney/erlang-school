-module(library_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NOT_SUBSCRIBED_ERROR,  {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(ALREADY_SUBSCRIBED_ERROR, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(ALREADY_EXISTS_ERROR,     {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).
-define(NO_ROOM_ERROR,                        {error, <<>>, <<"NO ROOM">>, <<>>}).
-define(DECODED_MSG,                 {leave, <<"Igor">>, <<"Hello">>, <<"rm1">>}).
-define(ENCODED_MSG, <<"{\"user\":\"Igor\",\"room\":\"rm1\",\"message\":\"Hello\",\"event\":\"leave\"}">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [atom()].

all() ->
    [
        encode_json,
        decode_json,
        symmetry,
        error_matching
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

-spec error_matching(C :: config()) ->
    term().

error_matching(_C) ->
    no_room = library_protocol:match_error(?NO_ROOM_ERROR),
    already_exists = library_protocol:match_error(?ALREADY_EXISTS_ERROR),
    already_joined = library_protocol:match_error(?ALREADY_SUBSCRIBED_ERROR),
    not_joined = library_protocol:match_error(?NOT_SUBSCRIBED_ERROR).
