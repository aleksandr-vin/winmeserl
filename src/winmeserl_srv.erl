%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012, Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @doc
%%% Port owner server
%%% @end
%%% Created : 18 Nov 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(winmeserl_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APPNAME, winmeserl).

-include("log.hrl").

-record(state, {port, heartbeat_tref, heartbeat_time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    HeartBeatTime = 10 * 1000, % ms
    case code:priv_dir(?APPNAME) of
        {error, _} ->
            ?error("~w priv dir not found", [?APPNAME]),
            {stop, 'priv-dir'};
        PrivDir ->
            {ok, Cmd} = application:get_env(?APPNAME, port_cmd),
            Port = open_port({spawn, filename:join([PrivDir, Cmd])},
                             [binary, {packet, 4}, exit_status]),
            {ok, TRef} = timer:exit_after(HeartBeatTime, 'port-heartbeat'),
            {ok, #state{port = Port,
                        heartbeat_time = HeartBeatTime,
                        heartbeat_tref = TRef}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {exit_status, _Status}} = Info,
            #state{port = Port} = State) ->
    {stop, Info, State};
handle_info({Port, {data, Data}},
            #state{port = Port} = State) when is_binary(Data) andalso
                                              size(Data) =:= 16 ->
    <<HWnd:32/integer-unsigned-big,
      WinMsg:32/integer-unsigned-big,
      WParam:32/integer-unsigned-big,
      LParam:32/integer-unsigned-big>> = Data,
    ?debug("Windows message, hwnd: ~p, message: ~p, wparam: ~p, lparam: ~p",
          [HWnd, WinMsg, WParam, LParam]),
    winmeserl_event:notify({HWnd, WinMsg, WParam, LParam}),
    {noreply, State};
handle_info({Port, {data, <<"debug:",Msg/binary>>}}, #state{port = Port} = State) ->
    ?debug("Port log: ~s", [Msg]),
    {noreply, State};
handle_info({Port, {data, <<"error:",Msg/binary>>}}, #state{port = Port} = State) ->
    ?error("Port log: ~s", [Msg]),
    {noreply, State};
handle_info({Port, {data, <<"heartbeat">>}},
            #state{port = Port, heartbeat_tref = OldTRef} = State) ->
    ?debug("Port heart beats!"),
    {ok, _OldTimer} =
        case OldTRef of
            undefined = U ->
                {ok, U};
            _ ->
                timer:cancel(State#state.heartbeat_tref)
        end,
    ?debug("Old timer result: ~p", [_OldTimer]),
    {ok, TRef} = timer:exit_after(State#state.heartbeat_time, 'port-heartbeat'),
    port_command(Port, <<"ok">>),
    {noreply, State#state{heartbeat_tref = TRef}};
handle_info({Port, {data, <<>>}} = Info,
            #state{port = Port} = State) ->
    ?warning("empty data packet received and server will stop now "
	     "(see issue #2 on github:"
	     " https://github.com/aleksandr-vin/winmeserl/issues/2)"),
    {stop, Info, State};
handle_info(_Info, State) ->
    ?warning("unknown message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
