%%% Motion Checking Sink.
%%%
%%%   Checks if the current erlcam frame is sufficiently different from the
%%%   previous so as to consider it to contain motion.
%%%
-module(motion_checker_sink).
-behaviour(libemp_sink).

-export([
  setup/1,
  process/3
]).

-define(THRESHOLD, 0.7).
-define(RGBY_THRESHOLD, 2).

%% @doc Create the sink by providing a blank initial state (i.e. empty frame).
setup( _Args ) ->
  {ok, <<>>}.

%% @doc Process a new frame from the webcam.
process( {erlcam,frame,NewFrame}, _BufRef, PrevFrame ) ->
  case has_motion( NewFrame, PrevFrame ) of
    %% If motion was see, keep the current frame as baseline but modify the
    %% event for down the stack to know it was motion.
    true -> {next, {image, NewFrame}, PrevFrame};

    %% If no motion was found, just update the current baseline with the
    %% current frame, and drop the event on the floor.
    false -> {drop, NewFrame}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Check if the two frames are different enough.
has_motion( _, <<>> ) -> false;
has_motion( NewFrame, PrevFrame ) ->
  %% Check for motion by ignorantly comparing the binaries. If they are
  %% N% different where N is over some threshold, then we consider it motion.
  Size = byte_size( NewFrame ),
  DiffCount = diff_count(NewFrame, PrevFrame, 0),
  PercentDiff = 1.0 - (DiffCount / Size),
  (PercentDiff > ?THRESHOLD).

%% @hidden
%% @doc Iterate through two binaries and compare each byte.
diff_count(<<>>, _, C) -> C;
diff_count(_, <<>>, C) -> C;
diff_count( <<A:1/binary,ARest/binary>>, <<B:1/binary,BRest/binary>>, C ) ->
  diff_count(ARest,BRest,do_incr(A,B,C)).
do_incr(<<R1:2,G1:2,B1:2,Y1:2>>, <<R2:2,G2:2,B2:2,Y2:2>>, CurCount ) ->
   Diff = abs( (R2-R1) + (G2-G1) + (B2-B1) + (Y2-Y1) ),
   case Diff > ?RGBY_THRESHOLD of
     true -> CurCount+1;
     false -> CurCount
   end.
