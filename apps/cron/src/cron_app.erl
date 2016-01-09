%%% LibEMP Cron Application -
%%%
%%%     An example application using LibEMP to implement a local event
%%%     based cron system. At the bare bones it starts LibEMP, a Time
%%%     Monitor, and a single Sync to run programs on events. You can
%%%     register programs with the sync via `cron:register_cmd/2'.
%%%
-module(cron_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
%% @doc Start the Cron supervisor and monitor the application.
start(_StartType, _StartArgs) ->
    cron_sup:start_link().

%% @private
%% @doc Clean up on application termination.
stop(_State) ->
    ok.

