# syntax=docker/dockerfile:1

FROM erlang:25.3.2-slim

ARG ENVIRONMENT=dev
# Debug mod for rebar3 release
# ARG DEBUG=1
ENV TZ=Etc/UTC

COPY . /build/

# RUN --mount=type=ssh,id=id_rsa \
RUN \
    export DEBIAN_FRONTEND=noninteractive TZ=${TZ} && \
    apt-get -y -q update && \
    apt-get -y -q install --no-install-recommends tzdata git ca-certificates \
    make libbrotli-dev libzstd-dev
RUN \
    cd /build && \
    rebar3 as ${ENVIRONMENT} release -d false -o /app && \
    cd /app && \
    rm -rf /build && \
    apt-get -y -q autoremove git && \
    apt-get -y -q remove make libbrotli-dev libzstd-dev

WORKDIR /app

EXPOSE 6001 8001

ENTRYPOINT [ "/app/ssl_impersonate/bin/ssl_impersonate" ]
CMD [ "foreground" ]
