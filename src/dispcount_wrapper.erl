-module(dispcount_wrapper).

-export([init/1,
         checkout/2,
         checkin/2]).

init(Opts) ->
    {ok, Pid} = mean_service_time:start_link(Opts),
    {ok, #{taken => false, pid => Pid}}.

checkout(_From, #{taken := true}=State) ->
    {error, busy, State};
checkout(From, #{taken := false, pid := Pid}=State) ->
    {ok, Pid, State#{taken := true}}.

checkin(Ref, #{taken := true}=State) ->
    {ok, State#{taken := false}};
checkin(Ref, State) ->
    {ignore, State}.
