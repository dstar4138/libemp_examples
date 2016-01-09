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
  signal_tick/1
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
handle_cast({register, Freq, Command}, State) -> 
    {noreply, add_prog(Freq,Command,State)};
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
%% Frequencies are described exactly like Crontab:
%%          minute hour day_of_month month day_of_week
%% Examples:
%%  *   *   *    *      *  = once every minute.
%%  1   *   *    *      *  = once an hour at *:01.
%%  *   2  */2   *      *  = At 2AM every even day of the month.
%% */5 9-16 * 1-5,9-12 1-5 = five minute intervals from 9AM to 4:55PM
%%                           on weekdays except during the summer
%%                           months.
-define(WHITESPACES, [$\n,$\t,$\s]).
-define(PLACE_LIMITS, [
  {0,59}, % Minutes
  {0,23}, % Hours
  {1,31}, % Days
  {1,12}, % Month
  {0,6}   % Day of week (Sunday=0)
]).

%% @hidden
%% @doc Add a program to the frequency mapper.
add_prog( Freq, Command, #state{freq_map = FreqMap} = State ) ->
  case compute_indexes(Freq) of
    {ok, Indexes} ->
      NewFreqMap = lists:foldl(freq_map_updater(Command), FreqMap, Indexes),
      State#state{freq_map = NewFreqMap};
    {error, _Reason} ->
      State
  end.

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
  Index = compute_index(Mins,Hrs,Day,Mnth,Wkdy),
  {ok, maps:get(Index,FreqMap,[])}.

%% @hidden
%% @doc Parse and compute the indexes of the Frequency Map based on a
%%   given frequency string. From a cold start this can take 7 seconds
%%   to compute the max number of indexes (at least on my machine).
%% @end
compute_indexes( Freq ) when is_list(Freq) ->
  Tokens = string:tokens(Freq, ?WHITESPACES),
  case length(Tokens) of
    5 ->
      Items = lists:map(fun parse_item/1,
                        lists:zip(Tokens,?PLACE_LIMITS)),
      enumerate_indexes(Items);
    N -> {error, {badlen,N,Tokens}}
  end.
parse_item({Item, {Low,High}}) ->
  lists:usort(
    lists:flatten(
      lists:map(fun(Group) -> decode_item_group(Group,Low,High) end,
                string:tokens(Item,",")))).
decode_item_group(Group,Low,High) ->
  case Group of
    "*" -> lists:seq(Low,High);
    [$*,$/|Interval] -> lists:seq(Low,High,charint(Interval));
    NumOrRange -> decode_num_range(NumOrRange,Low,High)
  end.
decode_num_range(NumRange,Low,High) ->
  case string:to_integer(NumRange) of
    {error,Reason} -> error(Reason);
    {GivenLow,Rest} ->
      {GivenHigh, Interval} =
          (case Rest of
             [$-,D1,D2,$/,D3] -> {charint(D1,D2),charint(D3)};
             [$-,D1,$/,D2] -> {charint(D1),charint(D2)};
             [$-,D1,D2] -> {charint(D1,D2),1};
             [$-,D1] -> {charint(D1),1};
             [] -> {GivenLow,1}
           end),
      if GivenLow >= Low andalso GivenLow =< GivenHigh andalso GivenHigh =< High ->
            lists:seq(GivenLow,GivenHigh,Interval);
         true -> error({badrange,NumRange,Low,High})
      end
  end.
charint(D) when is_list(D) ->
  case string:to_integer(D) of
    {error,Reason} -> error(Reason);
    {Int, []} -> Int;
    {_int,Rest} -> error({badchar,Rest})
  end;
charint(D) -> charint([D]).
charint(D1,D2) -> charint([D1,D2]).
compute_index(Mins,Hrs,Day,Mnth,Wkdy) ->
  % 0008192301 = August 19th as a Sunday at 23:01 local time.
  % 00         > Weekday
  %   08       > Month
  %     19     > Day
  %       23   > Hour
  %         01 > Minute
  Wkdy * 100000000 +
  Mnth * 1000000   +
  Day  * 10000     +
  Hrs  * 100       +
  Mins * 1.
enumerate_indexes([Mins,Hrs,Days,Mnts,Wkdys]) ->
  % Generate a set of indexes given all possible values
  % for each crontab item. i.e.
  % [ [1,2], [1], [1], [1], [1,2] ] => [ 0101010101, 0101010102, 0201010101, 0201010102 ]
  % foreach m in mins
  %   foreach h in hrs
  %     foreach d in days
  %       foreach n in mnts
  %         foreach w in wkdys
  %           Indexes.append(compute_index(m,h,d,b,w))
  Indexes = lists:foldl(fun(M, Is) ->
              lists:append(Is,
                lists:foldl(fun(H,Is2) ->
                  lists:append(Is2,
                    lists:foldl(fun(D,Is3) ->
                      lists:append(Is3,
                        lists:foldl(fun(N,Is4) ->
                          lists:append(Is4,
                            lists:foldl(fun(W,Is5) ->
                              [compute_index(M,H,D,N,W)|Is5]
                            end, [], Wkdys))
                        end, [], Mnts))
                    end, [], Days))
                end, [], Hrs))
            end, [], Mins),
  {ok, Indexes}.