%%% cron - 
%%%
%%%     An Application wrapper around a local LibEMP service to implement a 
%%%     simple cron. This can easily be extended to be distributed and run
%%%     commands across multiple machines.
%%% 
-module( cron ).

-export( [ start/0, stop/0 ] ).
-export( [ register_cmd/2 ] ).

-define(APP, cron).
-define(CRON_CONFIG(BufferName), [
        {monitor, BufferName, cron_monitor, []},
        {sink, cron_sink, []},
        {sink, cron_alarm_sink, [emit,log,drop]},
        {stack, BufferName, [cron_alarm_sink, cron_sink]}
]).

%% @doc Start the Cron service.
start() -> start( default ).

%% @doc Start the cron service and specify which buffer to use.
start( BufferName ) ->
    application:start( ?APP ),
    case libemp:ensure_started( ?CRON_CONFIG(BufferName) ) of
        ok    -> ok;
        Error ->
          application:stop( ?APP ),
          Error
    end.


%% @doc Stop the Cron service.
stop() ->
    %TODO: uninstall monitor and sink
    application:stop( cron ).

%% @doc Register a command with the Cron service. Note, this registers the 
%%   command to run on the local machine.
%% @end
register_cmd( Frequency, Command ) ->
    cron_service:register_cmd( Frequency, Command ).

