-module(driver).

-compile(export_all).

-import(mean_service_time, [set_service_time/1]).
-import(client, [set_rate/1]).

do() ->
    [ scenario(Backend) || Backend <- backends() ].

backends() ->
    [cuesport_pool,
     poolboy_no_overflow_pool,
     poolboy_block_pool,
     poolboy_overflow_pool,
     inaka_best_worker_pool,
     inaka_random_worker_pool,
     inaka_next_worker_pool,
     inaka_available_worker_pool,
     dispcount_hash_pool,
     dispcount_round_robin_pool].


scenario(Backend) ->
    scenario(Backend, #{pool_size => 10,
                        pool_name => worker_pool,
                        backend => Backend,
                        clients => 1000,
                        rate => 0.5,
                        mean_service_time => 10}).

scenario(Backend, #{rate := Rate, mean_service_time := Time}=Opts) ->
    io:format("Starting ~p!~n", [Backend]),
    ensure_metrics(),
    annotation("Backend " ++ atom_to_list(Backend) ++ " started"),
    set_rate(Rate),
    set_service_time(Time),
    start_backend(Opts),
    start_clients(Opts),
    timer:sleep(timer:minutes(5)),
    set_service_time(500),
    timer:sleep(timer:minutes(5)),
    set_service_time(5),
    timer:sleep(timer:minutes(5)),
    set_rate(10),
    timer:sleep(timer:seconds(30)),
    set_rate(0.5),
    timer:sleep(timer:minutes(5)),
    annotation("Backend " ++ atom_to_list(Backend) ++ " finished"),
    io:format("I'm done with ~p!~n", [Backend]),
    stop_clients(Opts),
    stop_backend(Opts).

start_backend(#{backend := Backend, pool_size := PoolSize}=Opts) ->
    Spec = {Backend,
            {Backend, start_link, [Opts]},
            permanent,
            5000,
            supervisor,
            [Backend]},
    ok = supervisor:check_childspecs([Spec]),
    exometer:update([service, pool_size], PoolSize),
    supervisor:start_child(worker_pools_sup, Spec).

stop_backend(#{backend := Backend}) ->
    ok = supervisor:terminate_child(worker_pools_sup, Backend),
    ok = supervisor:delete_child(worker_pools_sup, Backend).

start_clients(#{clients := Clients}=Opts) ->
    Spec = {client_sup,
            {client_sup, start_link, [Opts]},
            permanent,
            5000,
            supervisor,
            [client_sup]},
    ok = supervisor:check_childspecs([Spec]),
    exometer:update([client, pool_size], Clients),
    supervisor:start_child(worker_pools_sup, Spec).

stop_clients(_) ->
    ok = supervisor:terminate_child(worker_pools_sup, client_sup),
    ok = supervisor:delete_child(worker_pools_sup, client_sup).

ensure_metrics() ->
    catch exometer:new([service, pool_size], gauge),
    catch exometer:new([client, pool_size], gauge),
    catch exometer:new([client, rate], gauge),
    catch exometer:new([service, mean_service_time], gauge).

annotation(Msg) ->
    Line = "annotation value=\"" ++ Msg ++ "\" ",
    Req = {"http://192.168.99.100:8086/write?db=graphitedb", [], "application/binary", Line},
    {ok, _Result} = httpc:request(post, Req, [], []).
