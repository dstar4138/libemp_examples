%%% cron LibEMP Alarm Sink -
%%%
%%%   There may be cases where the tick event is not processed until way after
%%%   the minute is up. This could be cause for alarm, especially on systems
%%%   where we want something to happen every minute. Thus, we can emit alarms
%%%   to the buffer, log the error and/or drop the event. More advanced
%%%   processing could be done, but it is out of the scope of this example
%%%
-module(cron_alarm_sink).

%% LibEMP Sink API
-export([
  validate_configs/1,
  setup/1,
  process/3
]).

%% @doc Validate the arguments before they get passed into `setup/1'.
validate_configs( Args ) ->
  case lists:foldl( fun validate/2, [], Args ) of
    [] -> ok;
    Errors -> {error, Errors}
  end.

%% @doc Set up the Alarm based on the configurations.
setup( Args ) ->
  {ok, create_state( Args )}.

%% @doc Process a tick event, based on the configuratio (i.e. make sure the tick
%%    read off the queue is still valid).
%% @end
process( {tick,_,_,_,_,_}=Tick, BufferRef, State ) ->
  case cron_time:current_tick() of
    Tick -> alarm( Tick, BufferRef, State );
    _ -> next
  end.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================
-record( state, {emit=false, log=false, drop=false} ).
-define( ERROR_MSG, "LATE_MSG(~p), Unable to process CRON message in time.~n" ).

%% @hidden
%% @doc Validate a configuration.
validate( emit, CurProb ) -> CurProb;
validate( log, CurProb ) -> CurProb;
validate( drop, CurProb ) -> CurProb;
validate( Unknown, CurProb ) ->
  [{unknown_config, Unknown} | CurProb].

%% @hidden
%% @doc Create our state to say what actions to perform on Alarms.
create_state( Args ) -> create_state(Args, #state{}).
create_state( [], State )       -> {ok, State};
create_state( [emit|R], State ) -> create_state(R, State#state{emit=true});
create_state( [log |R], State ) -> create_state(R, State#state{log=true});
create_state( [drop|R], State ) -> create_state(R, State#state{drop=true}).

%% @hidden
%% @doc Perform the Alarm based on the state.
alarm( Tick, BufferRef, State ) ->
  emit( Tick, BufferRef, State ),
  log( Tick, State ),
  drop(State). % Must go last as it decides the `process/3' return value.
emit( Tick, BufferRef, #state{emit=true} ) ->
  libemp_buffer:give({alarm_tick, Tick}, BufferRef);
emit( _, _, _ ) -> ok.
log( Tick, #state{log=true} ) ->
  error_logger:error_info(io_lib:format(?ERROR_MSG, [Tick]));
log( _, _ ) -> ok.
drop( #state{drop=true} ) -> drop;
drop( _ ) -> next.
