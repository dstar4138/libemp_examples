%%% Cron Command Supervisor
%%%
%%%   A local supervisor for monitoring the Tasks that the cron
%%%   system starts up.
%%%
-module(cron_proc_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  supervise_commands/1
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor, and waits for commands to supervise.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Build and add a processor to the supervisor. It will immediately
%%   begin pulling from the buffer.
%% @end
supervise_commands( [] ) -> ok;
supervise_commands( [Cmd|Cmds] ) ->
    supervisor:start_child( ?MODULE, [Cmd] ),
    supervise_commands(Cmds).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize the supervisor as a simple-one-for-one style monitor.
%%   This will monitor the anonymous functions running the commands.
%% @end
init( _ ) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => processor,
                    restart => temporary,
                    start => {cron_process, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

