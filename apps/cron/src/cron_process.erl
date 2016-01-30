%%% Cron Command Process Wrapper
%%%
%%%   Wrap an OS task port in a gen_server.
%%%
-module(cron_process).
-behaviour(gen_server).

%% API
-export([
  start_link/1
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

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start_link( Command ) ->
  gen_server:start_link(?MODULE, [Command], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server around an Erlang Port.
init([Command]) ->
  process_flag( trap_exit, true ),
  make_and_link_process( Command ).

%% @private
%% @doc Handling call messages
handle_call(_Request, _From, State) -> {reply, {error, badarg}, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages. Namely details from the open port.
%TODO: save Reason in log.
handle_info({'EXIT',Pid,_Reason}, Pid) -> {stop, shutdown, noport};
handle_info({Port, {exit_status,_Reason}}, Port) -> {stop,shutdown,noport};
handle_info({Port, _UnhandledMsg}, Port) -> {noreply, Port};
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc Clean up service.
terminate(_Reason, _State) -> ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Wrap a local process or external port in a gen_server to monitor it.
make_and_link_process( Function ) when is_function(Function,0) ->
  Pid = spawn_link( Function ),
  {ok, Pid};
make_and_link_process( Command ) when is_binary( Command ) orelse is_list( Command ) ->
  case catch erlang:open_port({spawn,Command},[exit_status,{parallelism,true}]) of
    {'EXIT',Reason} -> {stop,Reason};
    Port -> {ok, Port}
  end.
