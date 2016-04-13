-module(client).

-behaviour(gen_server).

%% API
-export([start_link/1,
         set_rate/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_client/2]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

set_rate(Rate) ->
    exometer:update([client, rate], Rate),
    ok = application:set_env(worker_pools, rate, Rate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Opts]) ->
    ensure_metrics(),
    schedule_timeout(),
    {ok, Opts}.

handle_call({set_rate, Rate}, _From, State) ->
    {reply, ok, State#{rate := Rate}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #{backend := Backend, pool_name := PoolName}=State) ->
    spawn(?MODULE, start_client, [Backend, PoolName]),
    schedule_timeout(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_client(Backend, PoolName) ->
    Ref = erlang:make_ref(),
    try
        exometer:update([client, calls], 1),
        {Time, Ref} = timer:tc(fun() -> Backend:handle(PoolName, Ref) end),
        exometer:update([client, time], Time),
        exometer:update([client, successful_calls], 1)
    catch _:_ ->
        exometer:update([client, failed_calls], 1)
    end.

schedule_timeout() ->
    {ok, Rate} = application:get_env(worker_pools, rate),
    Timeout = erlang:round(1000 / Rate),
    erlang:send_after(Timeout, self(), timeout).

ensure_metrics() ->
    catch exometer:new([client, time], histogram),
    catch exometer:new([client, calls], spiral),
    catch exometer:new([client, failed_calls], spiral),
    catch exometer:new([client, successful_calls], spiral).
