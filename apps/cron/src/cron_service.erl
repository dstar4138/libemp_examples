%%% Cron Service Store -
%%%
%%%     Stores and calculates the commands to run at each tick of the cron.
%%%     Note that this is a local instance. We can start one of these up
%%%     on each node to have a distributed job service.
%%%
-module(cron_service).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  signal_tick/1,
  register_cmd/2
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

-record(state, { freq_map = #{} }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Signal that a tick happened, and get the commands that should run
%%   as part of it.
%% @end
signal_tick( Time ) ->
  gen_server:call(?MODULE, {tick,Time}).

%% @doc Add a command to execute based on the frequency string.
register_cmd( Freq, Command ) ->
  case cron_time:compute_indexes( Freq ) of
    {ok, Indexes} ->
      gen_server:cast(?MODULE, {register, Indexes, Command}),
      ok;
    {error, _Reason}=Err ->
      Err
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) -> {ok, #state{}}.

%% @private
%% @doc Handling call messages
handle_call({tick, Time}, _From, State) ->
    {reply, compute_progs( Time, State ), State};
handle_call(_Request, _From, State) -> {reply, {error, badarg}, State}.

%% @private
%% @doc Handling cast messages
handle_cast({register, Indexes, Command}, State) ->
    {noreply, add_prog(Indexes,Command,State)};
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc Clean up service.
terminate(_Reason, _State) -> ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Add a program to the frequency mapper.
add_prog( Indexes, Command, #state{freq_map = FreqMap} = State ) ->
  NewFreqMap = lists:foldl(freq_map_updater(Command), FreqMap, Indexes),
  State#state{freq_map = NewFreqMap}.

%% @hidden
%% @doc Creates a function which adds the given Command to all indexes
%%   of a frequency map.
%% @end
freq_map_updater(Command) ->
  fun(Index, FreqMap) ->
    CmdList = maps:get(Index, FreqMap, []),
    maps:update(Index,[Command|CmdList],FreqMap)
  end.

%% @hidden
%% @doc Compute what Commands to run during the Time tick.
compute_progs( {Mins,Hrs,Day,Mnth,Wkdy}, #state{freq_map = FreqMap} ) ->
  Index = cron_time:compute_index(Mins,Hrs,Day,Mnth,Wkdy),
  {ok, maps:get(Index,FreqMap,[])}.

