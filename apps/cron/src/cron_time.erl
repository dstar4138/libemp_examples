%%% cron time -
%%%   Creates the time tick events from the current system time and knows how
%%%   to parse Frequencies. Frequencies are described exactly like Crontab:
%%%                 minute hour day_of_month month day_of_week
%%%   Examples:
%%%     *   *   *    *      *  = once every minute.
%%%     1   *   *    *      *  = once an hour at *:01.
%%%     *   2  */2   *      *  = At 2AM every even day of the month.
%%%    */5 9-16 * 1-5,9-12 1-5 = five minute intervals from 9AM to 4:55PM
%%%                              on weekdays except during the summer
%%%                              months.
-module(cron_time).

%% API
-export([current_tick/0]).
-export([compute_index/5, compute_indexes/1]).

%% Our event structure that we generate.
-define(TICK_EVENT(Minutes,Hours,Day,Month,DayOfWeek),
          {tick,Minutes,Hours,Day,Month,DayOfWeek}).

-define(WHITESPACES, [$\n,$\t,$\s]).
-define(PLACE_LIMITS, [
  {0,59}, % Minutes
  {0,23}, % Hours
  {1,31}, % Days
  {1,12}, % Month
  {0,6}   % Day of week (Sunday=0)
]).

%% @doc Get the current "tick" event based on the system time.
current_tick() ->
  {{Year,Month,Day}, {Hour,Minute,_sec}} = calendar:local_time(),
  WkDay = case calendar:day_of_the_week(Year,Month,Day) of
            7 -> 0; % We want Sunday = 0 not 7.
            N -> N
          end,
  ?TICK_EVENT(Minute,Hour,Day,Month,WkDay).

%% @doc Given the elements of a Tick Event, compute the index into the map of
%%    Commands stored on `cron_service'.
%% @end
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

%%%===================================================================
%%% Internal Parsing Functionality
%%%===================================================================

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

enumerate_indexes([Mins,Hrs,Days,Mnts,Wkdys]) ->
  % Generate a set of indexes given all possible values
  % for each crontab item. i.e.
  % [ [1,2], [1], [1], [1], [1,2] ] =>
  %               [ 0101010101, 0101010102, 0201010101, 0201010102 ]
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