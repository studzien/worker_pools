-module(cuesport_pool).

-export([start_link/1,
         handle/2]).

start_link(#{pool_size := PoolSize,
             pool_name := PoolName}=Opts) ->
    cuesport:start_link(PoolName, PoolSize, [mean_service_time],
                        {mean_service_time, start_link}, 
                        {for_all, [Opts]}).

handle(PoolName, Something) ->
    mean_service_time:handle(cuesport:get_worker(PoolName), Something).
