-module(poolboy_block_pool).

-export([start_link/1,
         handle/2]).

start_link(#{pool_size := PoolSize,
             pool_name := PoolName}=Opts) ->
    PoolArgs = [{name, {local, PoolName}},
                {worker_module, mean_service_time},
                {size, PoolSize},
                {max_overflow, 0}],
    poolboy:start_link(PoolArgs, Opts).

handle(PoolName, Something) ->
    Worker = poolboy:checkout(PoolName, false),
    Result = mean_service_time:handle(Worker, Something),
    poolboy:checkin(PoolName, Worker),
    Result.
