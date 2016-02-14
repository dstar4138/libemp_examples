%%% LibEMP Motion App
%%%
%%%   Analyzes webcam images to see if the differences correspond to motion and
%%%   then saves and logs the images.
%%%
-module(motion).

-export([
  start/0,
  stop/0
]).

%% The group ErlCam publishes images to.
-define(MONITOR_GROUP, libemp_motion).

%% The interval in milliseconds we will check the image for motion.
-define(INTERVAL, 1000*10).

%% The directory to save motion capture images.
-define(SAVE_PATH, "_build/motion/").

%% Whether to just overwrite the same image file, or continuously generate more.
-define(OVERWRITE_STYLE, constant). % Replace with 'unique' if you want a new
                                    % image for each event.

%% The Wiring description for the LibEMP Motion App.
-define(LIBEMP_APP, [
  {buffer, motion, libemp_simple_buffer},
  {sink, logger, libemp_logger_sink},
  {sink, saver, {motion_saver_sink, [{path,?SAVE_PATH},?OVERWRITE_STYLE]}},
  {sink, checker, motion_checker_sink},
  % Wire up the Sink stack and the monitor to the Motion buffer.
  {monitor, motion_monitor, {pg2_monitor, [?MONITOR_GROUP]}, motion},
  {stack, [ checker, saver, logger ], motion}
]).

%% @doc Start the ErlCam application for capture webcam images, and
%%    the LibEMP Motion App for analyzing them.
%% @end
start() ->
  pg2:create( ?MONITOR_GROUP ),
  setup_erlcam(),
  setup_libemp().

%% @doc Stop the ErlCam application and ensure the Motion LibEMP App
%%    has shutdown. This leaves the LibEMP node running as other apps
%%    could be running.
%% @end
stop() ->
  erlcam:stop(),
  libemp:stop( ?MODULE ).

%%%==================================================================
%%% Internal Functionality
%%%==================================================================

%% @hidden
%% @doc Set up the erlcam application and make sure it starts
%%    broadcasting webcam images to a place that the LibEMP Motion
%%    App will be listening.
%% @end
setup_erlcam() ->
  erlcam:start(),
  erlcam:add_group(?MONITOR_GROUP, ?INTERVAL).

%% @hidden
%% @doc Set up the LibEMP subsystem and ensure the Motion App has
%%    started.
%% @end
setup_libemp() ->
  libemp:wire( ?MODULE, ?LIBEMP_APP ).