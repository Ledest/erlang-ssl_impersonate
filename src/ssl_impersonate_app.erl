-module(ssl_impersonate_app).

-behaviour(application).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).
-export([init/1]).

start(_StartType, _StartArgs) ->
    case code:get_mode() of
        interactive ->
            code:add_patha(path()),
            reload_modules(),
            supervisor:start_link(?MODULE, []);
        M ->
            ?LOG_CRITICAL("ssl_impersonate can work in interactive mode only, but node started in ~p mode", [M]),
            {error, non_interactive}
    end.

stop(_State) ->
    code:del_path(path()),
    reload_modules().

init(_) -> {ok, {#{}, []}}.

path() -> filename:join([code:priv_dir(ssl_impersonate), "ssl", "ebin"]).

reload_modules() ->
    Ext = code:objfile_extension(),
    lists:foreach(fun(F) ->
                      M = list_to_atom(filename:basename(F, Ext)),
                      code:is_loaded(M) =/= false andalso code:load_file(M)
                  end,
                  filelib:wildcard(filename:join(path(), [$*|Ext]))).
