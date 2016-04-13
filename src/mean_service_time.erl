-module(mean_service_time).

-behaviour(gen_server).

%% API
-export([start_link/1,
         set_service_time/1,
         handle/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(_Opts) ->
    gen_server:start_link(?MODULE, [], []).

set_service_time(ServiceTime) ->
    exometer:update([service, mean_service_time], ServiceTime),
    ok = application:set_env(worker_pools, mean_service_time, ServiceTime).

handle(Pid, Something) ->
    gen_server:call(Pid, {handle, Something}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    random:seed(os:timestamp()),
    ensure_metrics(),
    {ok, #{}}.

handle_call({handle, X}, _From, State) ->
    {ok, Time} = application:get_env(worker_pools, mean_service_time),
    Timeout = erlang:round(Time * 2 * random:uniform()),
    exometer:update([service, time], Timeout*1000),
    timer:sleep(Timeout),
    exometer:update([service, served], 1),
    report_queue_len(),
    {reply, X, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_metrics() ->
    catch exometer:new([service, time], histogram),
    catch exometer:new([service, served], spiral),
    catch exometer:new([service, queue_len], histogram).

report_queue_len() ->
    {message_queue_len, QLen} = erlang:process_info(self(), message_queue_len),
    exometer:update([service, queue_len], QLen).
