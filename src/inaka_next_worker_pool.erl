-module(inaka_next_worker_pool).

-export([start_link/1,
         handle/2]).

start_link(#{pool_size := PoolSize,
             pool_name := PoolName}=Opts) ->
    wpool:start_pool(PoolName, [{workers, PoolSize},
                                {worker, {mean_service_time, []}}]).

handle(PoolName, Something) ->
    wpool:call(PoolName, {handle, Something}, next_worker, infinity).
