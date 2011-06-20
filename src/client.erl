%%%-------------------------------------------------------------------
%%% @author Corecool <>
%%% @copyright (C) 2011, Corecool
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2011 by Corecool <>
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_server).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

-define(SYNC,sync).
-define(ASYNC,async).

%% API
%% client as a real client. Compair with the Server API, it has another parameter which called Pid.
-export([start_link/1,stop/1]).
-export([make_random_requests/3,get_random_requests/1,
	simulate/1]).
-export([visit_res/2]).

%% register the server. Client as the one of the server components.
-export([start_link/0,stop/0]).
-export([make_random_requests/2,get_random_requests/0,
	simulate/0]).
-export([visit_res/1]).  

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() | start_link(client) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(client) ->
    gen_server:start_link(?MODULE,[],[]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @spec stop() | stop(Pid) -> {stop,normal,State}
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid,stop).

stop() ->
    gen_server:cast(?SERVER,stop).
%%--------------------------------------------------------------------
%% @doc
%% Build the random requests to get the test results.
%%
%% @spec make_random_requests() | make_random_request(Pid) -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
make_random_requests(ResRange,ReqNum,Pid) ->
    gen_server:cast(Pid,{make_req,ResRange,ReqNum}).

make_random_requests(ResRange,ReqNum) ->
    gen_server:cast(?SERVER,{make_req,ResRange,ReqNum}).
%%--------------------------------------------------------------------
%% @doc
%% Get the random requests to verify the builder.
%%
%% @spec get_random_requests() | get_random_requests(Pid) -> [Requests]
%% @end
%%--------------------------------------------------------------------
get_random_requests(Pid) ->
    gen_server:call(Pid,get_reqs).

get_random_requests() ->
    gen_server:call(?SERVER,get_reqs).
%%--------------------------------------------------------------------
%% @doc
%% Simulate the resource requests.
%%
%% @spec simulate() | simulate(Pid) -> [Request Resource]
%% @end
%%--------------------------------------------------------------------
simulate(Pid) ->
    gen_server:call(Pid,simulate,infinity).

simulate() ->
    gen_server:call(?SERVER,simulate,infinity).
%%--------------------------------------------------------------------
%% @doc
%% visit the single resource.
%%
%% @spec visit_res(#resource{}) | visit_res(#resource{},Pid) -> Request Resource
%% @end
%%--------------------------------------------------------------------
visit_res(#resource{} = Res,Pid) ->
    gen_server:call(Pid,{visit,Res}).

visit_res(#resource{} = Res) ->
    gen_server:call(?SERVER,{visit,Res}).
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
    {ok, []}.

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
handle_call(simulate, _From, State) ->
    statistics(wall_clock),
    AsyncReply = loop_visit(State,[],?ASYNC),
    {_,AsyncTime} = statistics(wall_clock),
    timer:sleep(20000),
    ?debugFmt("ASync time is: ~pms ~n", [AsyncTime]),
    ?debugFmt("Cache Miss: ~p~n",
	      [monitor:get_cache_miss()]),
    monitor:reset_counter(),
    cache:clear(),
    check_lirs_init(),   
    check_monitor_reset(),
    statistics(wall_clock),
    SyncReply = loop_visit(State,[],?SYNC),
    {_,SyncTime} = statistics(wall_clock),
    ?debugFmt("Sync time is: ~pms ~n", [SyncTime]),
    ?debugFmt("Cache Miss: ~p~n",
	      [monitor:get_cache_miss()]),
    
    ?assertEqual(SyncReply,AsyncReply),
    {reply,SyncReply,[]};

handle_call({visit,Res}, _From, State) ->
    {reply,cache:visit_res(Res),State};

handle_call(get_reqs, _From, State) ->
    Reply = State,
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
handle_cast(stop,State) ->
    {stop,normal,State};

handle_cast({make_req,ResRange,ReqNum}, State) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    NewState = build_reqs(ResRange,ReqNum,State),
    {noreply, NewState}.

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
handle_info(_Info, State) ->
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
build_reqs(_ResRange,0,State) ->
    State;
build_reqs(ResRange,ReqNum,State) ->
    Range = random:uniform(10),
    ID = if
    	     Range =< 8 ->
    		 random:uniform(round(ResRange* 0.2));
    	     true ->
    		 random:uniform(round(ResRange * 0.8)) +
    		     round(ResRange * 0.2)
    	 end,
    NewState = [#resource{id = ID} | State],
    build_reqs(ResRange,ReqNum - 1,NewState).
    
loop_visit([],VisitRes,Flag) when
      Flag ==?ASYNC; Flag ==?SYNC ->
    VisitRes;
loop_visit([Res | OtherRes],VisitRes,Flag) when
      Flag ==?ASYNC; Flag ==?SYNC ->
    Resource = cache:visit_res(Res,Flag),
    loop_visit(OtherRes,[Resource | VisitRes],Flag).

check_lirs_init() ->    
    ?assertMatch([{lirQueue,[]}],
		 ets:lookup(lirsRam,lirQueue)),
    ?assertMatch([{hirQueue,[]}],
		 ets:lookup(lirsRam,hirQueue)),
    ?assertEqual(3,ets:info(lirsRam,size)),
    ?assertEqual(0,ets:info(visitTab,size)),
    ?assertEqual(0,ets:info(cacheTab,size)).

check_monitor_reset() ->
    ?assertEqual(0,monitor:get_cache_miss()),
    ?assertEqual(0,monitor:get_res_miss()),
    ?assertEqual(0,monitor:get_notify_count()).
