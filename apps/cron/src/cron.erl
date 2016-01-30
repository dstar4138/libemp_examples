%%% cron - 
%%%
%%%     An Application wrapper around a local LibEMP service to implement a 
%%%     simple cron. This can easily be extended to be distributed and run
%%%     commands across multiple machines.
%%%
%%%     NOTE: For testing purposes only! Frequency String has the Weekday
%%%     matching purposefully disabled.
%%%
-module( cron ).

-export( [ start/0, stop/0 ] ).
-export( [ register_cmd/2 ] ).
-export( [ example_register/0 ]).

-define(APP, cron).
-define(CRON_CONFIG(BufferName), [
        {monitor, BufferName, libemp_timer_monitor, []},
        {sink, cron_sink, []},
        {sink, cron_alarm_sink, [emit,log,drop]},
        {stack, [cron_alarm_sink, cron_sink], BufferName}
]).

%% @doc Start the Cron service on the local node.
start() -> start( default ).

%% @doc Start the cron service by loading it on the local node. This ensures
%%   the node has been started, and the configs are loaded onto it.
%% @end
start( BufferName ) ->
  application:start( ?APP ),
  libemp:wire( ?APP, ?CRON_CONFIG(BufferName) ).

%% @doc Stop the Cron service on the local node.
stop() ->
  libemp:stop( ?APP ),
  application:stop( ?APP ).

%% @doc Register a command with the Cron service. Note, this registers the 
%%   command to run on the local node.
%% @end
register_cmd( Frequency, Command ) ->
    cron_service:register_cmd( Frequency, Command ).

%% @doc Register a logger for every tick, and an external command on every other.
example_register() ->
  EveryTime  = "* * * * *",
  EveryOther = "*/2 * * * *",
  cron:register_cmd(EveryTime, fun() -> error_logger:info_report("RAN_LOGGER!") end),
  cron:register_cmd(EveryOther, "sleep 10; echo hithere"),
  io:format("Added internal and external command for each and every other tick.").