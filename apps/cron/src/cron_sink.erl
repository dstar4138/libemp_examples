%%% Cron LibEMP Event Sink.
%%%
%%%   Attach to the local LibEMP instance and monitor for tick events.
%%%   To expand capabilities, we could have it push new events to the
%%%   buffer, or modify the event to show that commands were triggered
%%%   due to it. For example, a sink stacked on it could validate the
%%%   quantity of started tasks and trigger more events based on overrun.
%%%   All without effecting the ticking and event processing mechanisms
%%%   which can be started on as many systems as desired.
%%%
-module(cron_sink).
-behaviour(libemp_sink).

%% LibEMP Sink API
-export([
  process/3
]).

%% @doc On a tick, start the commands and as our supervisor to monitor them.
process( {tick,Min,Hr,Day,Mnth,Wkdy}, _BufferRefrence, _State ) ->
  {ok,Cmds} = cron_service:signal_tick({Min,Hr,Day,Mnth,Wkdy}),
  cron_proc_sup:supervise_commands(Cmds),
  next. % Signal that any other sink in the stack can have the tick event.

