FROM erlang:21.0 as build
COPY ./ /build
WORKDIR /build
RUN rebar3 compile
RUN rebar3 do eunit, ct
RUN rm -rf _build/default/rel
RUN rebar3 release
FROM erlang:21.0
WORKDIR /opt/chat
COPY --from=build /build/_build/default/rel/chat .
CMD bin/chat foreground
