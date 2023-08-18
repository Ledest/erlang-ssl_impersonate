-module(ssl_impersonate_app).

-behaviour(application).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).
-export([init/1]).

start(_StartType, _StartArgs) ->
    code:add_patha(filename:join([code:priv_dir(ssl_impersonate), "ssl", "ebin"])),
    reload_modules(),
    supervisor:start_link(?MODULE, []).

stop(_State) -> ok.

init(_) -> {ok, {#{}, []}}.

reload_modules() ->
    Ext = code:objfile_extension(),
    lists:foreach(fun(F) ->
                      M = list_to_atom(filename:basename(F, Ext)),
                      code:is_loaded(M) =/= false andalso code:load_file(M)
                  end,
                  filelib:wildcard(filename:join([code:priv_dir(ssl_impersonate), "src", "ebin", [$*|Ext]]))).
