%%% Cron LibEMP Timer Monitor.
%%%
%%%   A Locally running LibEMP Monitor which generates `tick' events on the minute
%%%   (i.e. when the wall-clock changes). Multiple entities can listen for this
%%%   event, however note that the only contracts this module has with the outside
%%%   world is the structure of the 'TICK_EVENT' and API used to trigger the event.
%%%
-module(cron_monitor).
-behaviour(libemp_monitor).

%% LibEMP Monitor API
-export([ describe/1, setup/3, destroy/2 ]).

%% Private exports
-export([loop/2]).

%% LibEMP Monitor Definition.
-define(MONITOR_NAME, "cron").
-define(MONITOR_CONFIG, []).

%% A fraction of a minute (1/6th or 10 seconds) seems like a good frequency to
%% validate whether the minute was up or not.
-define(FRACTION, 10*1000).

%%% ==========================================================================
%%% LibEMP Monitor Callbacks
%%% ==========================================================================

%% @doc Return default name and libemp configuration for the Cron monitor. We
%%   do not need to check the host to see if we are able to run as there are no
%%   requirements.
%% @end
describe( _Args ) ->
  {ok, ?MONITOR_NAME, ?MONITOR_CONFIG}.

%% @doc Initialize our Monitor.
setup( _Args, _Config, EMP ) ->
  Pid = spawn(?MODULE,loop,[EMP, cron_time:current_tick()]),
  {ok,Pid}.

%% @doc Shutdown our Monitor by killing the timer.
destroy( _Reason, Pid ) ->
  Pid ! shutdown.

%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @private
%% @doc Our looping function, we check the local time, and if a minute
%%   has passed on the wall-clock, trigger an event.
%% @end
loop(EMP, PreviousTick) ->
  receive
    shutdown ->
      ok;
    _ -> % Ignore a message but force a trigger check.
      check_if_trigger(EMP,PreviousTick)
  after % After every fraction of a minute check if the tick time changed.
    ?FRACTION ->
      check_if_trigger(EMP,PreviousTick)
  end.

%% @hidden
%% @doc Check if the event should trigger, then jump back into `loop/2'.
check_if_trigger(EMP,PreviousTick) ->
  case cron_time:current_tick() of
    PreviousTick -> ok;
    NewTick -> libemp_monitor_api:emit(NewTick, EMP)
  end,
  ?MODULE:loop(EMP, PreviousTick).

