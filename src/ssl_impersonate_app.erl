-module(ssl_impersonate_app).

-behaviour(application).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).
-export([init/1]).

start(_StartType, _StartArgs) ->
    code:add_patha(filename:join([code:priv_dir(ssl_impersonate), "ssl", "ebin"])),
    supervisor:start_link(?MODULE, []).

stop(_State) -> ok.

init(_) -> {ok, {#{}, []}}.
