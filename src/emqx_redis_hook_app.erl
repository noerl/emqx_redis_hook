%%%-------------------------------------------------------------------
%% @doc emqx_redis_hook public API
%% @end
%%%-------------------------------------------------------------------

-module(emqx_redis_hook_app).

-behaviour(application).
-emqx_plugin(?MODULE).

-define(APP, emqx_redis_hook).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Opts = [
        {init_nodes,[
            {"127.0.0.1",6379}
        ]},
        {pool_size, 5},
        {pool_max_overflow, 0}
    ],
    {ok, _RedisPid} = eredis_cluster:start_pool(eredis_pool, Opts),
    {ok, Sup} = emqx_redis_hook_sup:start_link(),
    ?APP:register_metrics(),
    ?APP:load(),
    emqx_redis_hook_cfg:register(),
    {ok, Sup}.

stop(_State) ->
    ok.

%% internal functions
