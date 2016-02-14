%%% Generic Image Saver.
%%%
%%%   Given the JPEG Binary of an image, we save a unique image in a provided
%%%   save path. For the 'Motion' app, we use this to save pictures of motion.
%%%
-module(motion_saver_sink).
-behaviour(libemp_sink).

-export([
  setup/1,
  process/3
]).

-record(state,{path,filename_fun}).

%% @doc Create the sink by providing the path to save pictures in.
setup( Args ) ->
  State = build_state( #state{}, Args ),
  filelib:ensure_dir(State#state.path),
  {ok, State}.

%% @doc Save an image event in the save path.
process( {image,Binary}, _BufRef, State ) ->
  FileName = get_filename(State),
  Path = filename:join( State#state.path, FileName ),
  file:write_file(Path, Binary),
  {next, {motion_captured, Path}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Abstract around the state to get the file name to save the image as.
get_filename( #state{filename_fun = undefined} ) ->
  constant_filename_fun(); %% Safer on the tester's disk-space.
get_filename( #state{filename_fun = Fun} ) ->
  Fun().

%% @hidden
%% @doc Override the state object based on the user-defined arguments.
build_state( State, [] ) -> State;
build_state( State, [{path,SavePath}|Args] ) ->
  build_state( State#state{path=SavePath}, Args );
build_state( State, [constant|Args] ) ->
  build_state( State#state{filename_fun = fun constant_filename_fun/0}, Args);
build_state( State, [unique|Args] ) ->
  build_state( State#state{filename_fun = fun unique_filename_fun/0}, Args);
build_state( State, [_|Args] ) ->
  build_state( State, Args ).

%% @hidden
%% @doc By default the sink will save the image to the same file name each time.
constant_filename_fun() -> "watchme.jpg".

%% @hidden
%% @doc The wiring config can specify 'unique' which will force new files to be
%%  written for each new event.
%% @end
unique_filename_fun() ->
  io_lib:format("~p.jpg",[erlang:unique_integer([positive])]).
