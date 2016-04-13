-module(client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, all/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

all() ->
    [ Pid || {_, Pid, _, _} <- supervisor:which_children(?SERVER) ].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([#{clients := Clients}=Opts]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100000,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [ {worker_name(I), {client, start_link, [Opts]},
                  Restart, Shutdown, Type, [client]}
                 || I <- lists:seq(1, Clients) ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
worker_name(I) ->
    list_to_atom("client_" ++ integer_to_list(I)).
