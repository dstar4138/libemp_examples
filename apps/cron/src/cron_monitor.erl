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
-export([
  register/0,
  init/2, uninit/1
]).

%% Private exports
-export([loop/2]).

-define(MONITOR_VERSION,"1.0.0").

%% Our event structure that we generate.
-define(TICK_EVENT(Minutes,Hours,Day,Month,DayOfWeek),
          {tick,Minutes,Hours,Day,Month,DayOfWeek}).

%% A fraction of a minute (1/3rd or 20 seconds) seems like a good frequency to
%% validate whether the minute was up or not.
-define(FRACTION, 20*1000).

%%% ==========================================================================
%%% LibEMP Monitor Callbacks
%%% ==========================================================================

%% @doc Register the `tick' event cron generates every minute.
register() ->
  MonitorName = cron,
  Events = [
    % It has an arity of 5, and does not need any special treatment.
    {tick, 5, []}
  ],
  Actions = [
    % This Monitor has no actions that should be opened up to LibEMP.
  ],
  Configurations = [
    % Default configurations should be fine.
  ],
  {MonitorName, ?MONITOR_VERSION, Actions, Events, Configurations}.

%% @doc Initialize our Monitor.
init(BufferRefrence,_Args) ->
  Pid = spawn(?MODULE,loop,[BufferRefrence, current_tick()]),
  {ok,Pid}.

%% @doc Shutdown our Monitor by killing the timer.
uninit(Pid) ->
  Pid ! shutdown.

%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @private
%% @doc Our looping function, we check the local time, and if a minute
%%   has passed on the wall-clock, trigger an event.
%% @end
loop(BufferReference, PreviousTick) ->
  receive
    shutdown ->
      ok;
    _ -> % Ignore a message but force a trigger check.
      check_if_trigger(BufferReference,PreviousTick)
  after % After every fraction of a minute check if the tick time changed.
    ?FRACTION ->
      check_if_trigger(BufferReference,PreviousTick)
  end.

%% @hidden
%% @doc Check if the event should trigger, then jump back into `loop/2'.
check_if_trigger(BufferReference,PreviousTick) ->
  case current_tick() of
    PreviousTick -> ok;
    NewTick -> libemp_monitor:trigger(BufferReference, NewTick)
  end,
  ?MODULE:loop(PreviousTick).

%% @hidden
%% @doc Get the current "tick" event based on the system time.
current_tick() ->
  {{Year,Month,Day}, {Hour,Minute,_sec}} = calendar:local_time(),
  WkDay = case calendar:day_of_the_week(Year,Month,Day) of
            7 -> 0; % We want Sunday = 0 not 7.
            N -> N
          end,
  ?TICK_EVENT(Minute,Hour,Day,Month,WkDay).
