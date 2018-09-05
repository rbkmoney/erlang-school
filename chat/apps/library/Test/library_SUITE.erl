-module(library_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type groupName() :: room_manager_SUITE:groupName().
-type config() :: room_manager_SUITE:config().

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, groupName()}].

all() ->
    [
        {group, basic_interactions}
    ].

-spec groups() ->
    [{groupName(), [sequence], [atom()]}].

groups() ->
    [
        {basic_interactions, [sequence], [
            encodeJson,
            decodeJson,
            symmetry
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    C1 = [
            {source, {left, <<"Igor">>, <<"Hello">>, <<"rm1">>}},
            {
                encoded,
                <<"{\"user\":\"Igor\",\"room\":\"rm1\",\"message\":\"Hello\",\"event\":\"left\"}">>
            }
         ] ++ C,
    application:ensure_all_started(library),
    application:start(library),
    C1.

-spec end_per_suite(C :: config()) ->
    config().

end_per_suite(C) ->
    application:stop(library),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec encodeJson(C :: config()) ->
    config().

encodeJson(C) ->
    SourceMessage = ?config(source, C),
    Json = ?config(encoded, C),
    Json = protocol:encode(SourceMessage),
    C.

-spec decodeJson(C :: config()) ->
    config().

decodeJson(C) ->
    SourceMessage = ?config(source, C),
    Json = ?config(encoded, C),
    SourceMessage = protocol:decode(Json),
    C.

-spec symmetry(C :: config()) ->
    config().

symmetry(C) ->
    SourceMessage = ?config(source, C),
    SourceMessage = protocol:decode(protocol:encode(SourceMessage)),
    C.
