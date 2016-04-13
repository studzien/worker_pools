-module(dispcount_hash_pool).

-export([start_link/1,
         handle/2]).

start_link(#{pool_size := PoolSize,
             pool_name := PoolName}=Opts) ->
    ok = dispcount:start_dispatch(PoolName,
                                  {dispcount_wrapper, []},
                                  [{restart, permanent},
                                   {shutdown, 4000},
                                   {maxr, 10},
                                   {maxt, 60},
                                   {dispatch_mechanism, hash},
                                   {resources, PoolSize}]).

handle(PoolName, Something) ->
    {ok, Info} = dispcount:dispatcher_info(PoolName),
    {ok, Reference, Resource} = dispcount:checkout(Info),
    Response = mean_service_time:handle(Resource, Something),
    dispcount:checkin(Info, Reference, Resource),
    Response.
