FROM borgespires/erlang

WORKDIR tmp

COPY . .
CMD rebar3 as prod release -o /artifacts