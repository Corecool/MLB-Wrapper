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

%% API
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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @spec stop() -> {stop,normal,State}
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER,stop).
%%--------------------------------------------------------------------
%% @doc
%% Build the random requests to get the test results.
%%
%% @spec make_random_requests() -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
make_random_requests(ResRange,ReqNum) ->
    gen_server:cast(?SERVER,{make_req,ResRange,ReqNum}).
%%--------------------------------------------------------------------
%% @doc
%% Get the random requests to verify the builder.
%%
%% @spec get_random_requests() -> [Requests]
%% @end
%%--------------------------------------------------------------------
get_random_requests() ->
    gen_server:call(?SERVER,get_reqs).
%%--------------------------------------------------------------------
%% @doc
%% Simulate the resource requests.
%%
%% @spec simulate() -> [Request Resource]
%% @end
%%--------------------------------------------------------------------
simulate() ->
    gen_server:call(?SERVER,simulate).
%%--------------------------------------------------------------------
%% @doc
%% visit the single resource.
%%
%% @spec visit_res() -> Request Resource
%% @end
%%--------------------------------------------------------------------
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
    Reply = loop_visit(State,[]),
    {_,Time} = statistics(wall_clock),
    ?debugFmt("Time is: ~pms ~n", [Time]),
    {reply,Reply,[]};
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
    %% Range = random:uniform(10),
    %% ID = if
    %% 	     Range =< 8 ->
    %% 		 random:uniform(round(ResRange* 0.2));
    %% 	     true ->
    %% 		 random:uniform(round(ResRange * 0.8)) +
    %% 		     round(ResRange * 0.2)
    %% 	 end,
    ID = random:uniform(ResRange),
    NewState = [#resource{id = ID} | State],
    build_reqs(ResRange,ReqNum - 1,NewState).
    
loop_visit([],VisitRes) ->
    VisitRes;
loop_visit([Res | OtherRes],VisitRes) ->
    Resource = cache:visit_res(Res),
    loop_visit(OtherRes,[Resource | VisitRes]).
    
