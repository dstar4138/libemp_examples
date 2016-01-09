%%% Cron Service Supervisor
%%%
%%%   Start up the Command registry (to keep track of the commands
%%%   to run), and start the supervisor which will keep track of
%%%   all the commands when they start up.
%%%
-module(cron_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Start up the Command registry and the Task process supervisor.
init([]) ->
  {ok, { #{ strategy => one_for_one,
            intensity => 0, % No restarts,
            period    => 1  % within the timeframe of 1 second.
          },
          [
            #{ id => cron_proc_sup, start => {cron_proc_sup,start_link,[]}, type => supervisor },
            #{ id => cron_service,  start => {cron_service,start_link,[]},  type => worker }
          ]}}.
