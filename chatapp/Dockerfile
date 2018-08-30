#BUILD

FROM erlang:21.0.3 as build

COPY ./ /build
WORKDIR /build

RUN rebar3 compile
RUN rebar3 do eunit, ct

#RELEASE

RUN rebar3 as prod release

FROM erlang:21.0.3

WORKDIR /opt/chatapp
COPY --from=build /build/_build/prod/rel/chatapp .

#RUN

CMD bin/chatapp foreground