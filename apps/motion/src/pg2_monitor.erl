%%% A General Process Group Monitor
%%%
%%%   Converts Process Group communication into LibEMP Monitor events.
%%%
-module(pg2_monitor).
-behaviour(gen_server).
-behaviour(libemp_monitor).

-export([
  setup/3,
  destroy/3
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, { group, emp }).

%%%===================================================================
%%% Monitor API
%%%===================================================================

setup( Args, _Config, EMP ) ->
  Group = get_pg2_group( Args ),
  gen_server:start_link( ?MODULE, [Group, EMP], [] ).

destroy( Reason, Pid, _EMP ) ->
  gen_server:cast(Pid, {shutdown, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server state.
init( Args ) -> {ok, build_state( Args )}.

%% @private
%% @doc Handling API Messages. This is unused presently.
handle_cast({shutdown, _Reason}=Res, State) -> {stop, Res, State};
handle_cast(_Request, State) -> {noreply, State}.
handle_call(_Request, _From, State) -> {reply, {error, badarg}, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(Event, State) -> emit_event(Event,State).

%% @private
%% @doc Handle termination and failure callbacks on the monitored service.
terminate( _Reason, _State) -> ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

get_pg2_group( [Group] ) -> Group.

build_state([Group, EMP]) ->
  pg2:join(Group, self()),
  #state{group = Group, emp = EMP}.

emit_event(Event, #state{emp=EMP}=State) ->
  libemp_monitor_api:emit(Event, EMP),
  {noreply, State}.