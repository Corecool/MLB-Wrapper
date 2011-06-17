%%%-------------------------------------------------------------------
%%% @author Corecool <>
%%% @copyright (C) 2011, Corecool
%%% @doc
%%%
%%% @end
%%% Created : 31 May 2011 by Corecool <>
%%%-------------------------------------------------------------------
-module(lirs).

-behaviour(gen_server).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").
-include("../include/cacheItem.hrl").

%% API
-export([start_link/0,start_link/1,start_link/2,stop/0]).
-export([visit_res/1,visit_res/2,clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(SYNC,sync).
-define(ASYNC,async).


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
%% 供LIRS有初始数据时测试使用
start_link(test) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  test,[]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  [],[]).

start_link(CacheSize,LirPercent) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  {CacheSize,LirPercent},[]).
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
%% Change the LIRS server status through resource visiting.
%%
%% @spec visit_res(ID | ?ASYNC) -> {noreply,State}
%% @spec visit_res(ID,?SYNC) -> {NewID,OldID} | NewID | true
%% @end
%%--------------------------------------------------------------------
visit_res(ID) when is_integer(ID) ->
    gen_server:cast(?SERVER,{visit,ID}).

visit_res(ID,?SYNC) when is_integer(ID) ->
    gen_server:call(?SERVER,{visit,ID});
visit_res(ID,?ASYNC) when is_integer(ID) ->
    ?MODULE:visit_res(ID).

%%--------------------------------------------------------------------
%% @doc
%% Reset the LIRS status. This Function is used for restart the LIRS algorithm.
%%
%% @spec clear() -> ok
%% @end
%%--------------------------------------------------------------------
clear() ->
    gen_server:call(?SERVER,clear).

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
%% 供LIRS有初始数据时测试使用
init(test) ->
    create_tables(),
    init_tables(test),
    {ok,ok};
init({CacheSize,LirPercent}) ->
    create_tables(),
    init_tables(CacheSize,LirPercent),
    {ok,ok};
init(_Args) ->
    create_tables(),
    init_tables(),
    {ok,ok}.

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
handle_call({visit,ID}, _From, State) ->
    Reply = visit_resource(ID,?SYNC),    
    {reply, Reply, State};
handle_call(clear, _From, State) ->
    reset_status(),
    {reply,ok,State}.

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
handle_cast({visit,ID},State) ->
    visit_resource(ID,?ASYNC),
    {noreply,State};
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
terminate(normal,_State) ->
    ok;

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
init_tables(test) ->
    Res1 = #cacheItem{id = 1,status = lir},
    Res2 = #cacheItem{id = 2,status = non_resident},
    Res3 = #cacheItem{id = 3,status = hir},
    Res4 = #cacheItem{id = 4,status = lir},
    Res5 = #cacheItem{id = 5,status = hir},
    Res6 = #cacheItem{id = 6,status = non_resident},
    Res8 = #cacheItem{id = 8,status = lir},
    Res9 = #cacheItem{id = 9,status = non_resident},
    LirQueue = [Res5,Res3,Res2,Res1,Res6,Res9,Res4,Res8],
    ets:insert(lirsRam,{lirQueue,LirQueue}),
    HirQueue = [Res5,Res3],
    ets:insert(lirsRam,{hirQueue,HirQueue}),
    ets:insert(lirsRam,{1,lir}),
    ets:insert(lirsRam,{2,non_resident}),
    ets:insert(lirsRam,{3,hir}),
    ets:insert(lirsRam,{4,lir}),
    ets:insert(lirsRam,{5,hir}),
    ets:insert(lirsRam,{6,non_resident}),
    ets:insert(lirsRam,{8,lir}),
    ets:insert(lirsRam,{9,non_resident}),
    ets:insert(lirsRam,{conf,{size,5},{lirPercent,0.6}}).

create_tables() ->
    ets:new(visitTab,[named_table]),
    ets:new(lirsRam,[named_table]).

init_tables() ->
    ets:insert(lirsRam,{lirQueue,[]}),
    ets:insert(lirsRam,{hirQueue,[]}),
    ets:insert(lirsRam,{conf,{size,5},{lirPercent,0.6}}).

init_tables(CacheSize,LirPercent) ->
    ets:insert(lirsRam,{lirQueue,[]}),
    ets:insert(lirsRam,{hirQueue,[]}),
    ets:insert(lirsRam,{conf,{size,CacheSize},
			{lirPercent,LirPercent}}).
    
lookup(Key) ->
    ets:lookup(lirsRam,Key).

lookup(Key,Pos) ->
    ets:lookup_element(lirsRam,Key,Pos).

update(ID,Status) when is_integer(ID) ->
    ets:insert(lirsRam,{ID,Status}),
    #cacheItem{id = ID,status = Status};
update(Key,Status) when is_atom(Key) ->
    ets:insert(lirsRam,{Key,Status}).

update(Key,ID,Status) when
      is_integer(ID),is_atom(Key),is_atom(Status) ->
    Queue = lookup(Key,2),
    NewQueue = 
	lists:map(
	  fun(#cacheItem{} = X) ->
		  if
		      X#cacheItem.id == ID ->
			  X#cacheItem{status = Status};
		      true -> X
		  end end,Queue),
    update(Key,NewQueue).    				 
    
%% 下面为业务逻辑方法，上面为持久化方法（涉及ets）
%% 下面有ets参与到注入代码，其于LIRS无关，用于缓存异步通知
get_cur_lirs_num() ->
    [{lirQueue,LirQueue}] = lookup(lirQueue),
    length(LirQueue).

get_lir_cache_size() ->
    [{conf,{size,S},{lirPercent,P}}] = lookup(conf),
    round(S * P - 0.5).

get_total_cache_size() ->
    {size,TotalSize} = lookup(conf,2),
    TotalSize.

get_res_prev_status(ID) ->
    case lookup(ID) of
	[] ->
	    non_resident;
	[{ID,Value}]->
	    Value
    end.

move_top(#cacheItem{} = Item,Key) when
      is_atom(Key) ->
    remove_from_queue(Item,Key),
    enter_queue(Item,Key),
    ok.

move_top(#cacheItem{id = ID} = Item,Key,NewState) when
      is_atom(Key),is_atom(NewState) ->
    remove_from_queue(Item,Key),
    NewItem = update(ID,NewState),
    enter_queue(NewItem,Key),
    ok.


remove_from_queue(#cacheItem{} = Item,Key) when
      is_atom(Key) ->
    Queue = lookup(Key,2),
    NewQueue = lists:delete(Item,Queue),
    update(Key,NewQueue),
    ok.

remove_oldest_hiritem() ->    
    OldestHirItem = lists:last(lookup(hirQueue,2)),
    update(OldestHirItem#cacheItem.id,non_resident),
    remove_from_queue(OldestHirItem,hirQueue),
    change_state_in_lirqueue(OldestHirItem,non_resident),
    %% 注入代码
    ets:insert(visitTab,{oldID,OldestHirItem#cacheItem.id}).

change_state_in_lirqueue(#cacheItem{} = Item,State) ->
    update(lirQueue,Item#cacheItem.id,State).

is_full_hirqueue() ->
    HirQueueSize = get_total_cache_size() - 
	get_lir_cache_size(),
    length(lookup(hirQueue,2)) >= HirQueueSize.

enter(#cacheItem{} = Item,Key) when
      Key == hirQueue; Key == lirQueue ->
    Queue = lookup(Key,2),
    NewQueue = [Item | Queue],
    update(Key,NewQueue),
    ok.

before_enter_hir_queue() ->
    HirQueueFull = is_full_hirqueue(),
    if
	HirQueueFull == true ->
	    remove_oldest_hiritem();
	true -> ok
    end.
    
enter_queue(#cacheItem{} = Item,hirQueue) ->
    before_enter_hir_queue(),
    enter(Item,hirQueue);
enter_queue(#cacheItem{} = Item,lirQueue) ->
    enter(Item,lirQueue).

%% 最初阶段，LIR队列未满。
initial_stage(ID) ->
    case lists:filter(fun(#cacheItem{} = X) ->
		      X#cacheItem.id == ID end,
	       lookup(lirQueue,2)) of
	[Item] ->
	    move_top(Item,lirQueue);
	[] ->
	    ets:insert(visitTab,{newID,ID}), %%注入代码
	    Item = update(ID,lir),
	    enter_queue(Item,lirQueue)
    end.
	   
%% 访问LIR资源。
access_lir(ID) ->
    InitLirQueue = lookup(lirQueue,2),
    [Item] = lists:filter(
	       fun(#cacheItem{} = X) ->
		       X#cacheItem.id == ID andalso 
			   X#cacheItem.status == lir end,
	       InitLirQueue),
    move_top(Item,lirQueue),
    OldestLirItem = lists:last(InitLirQueue),
    if
	OldestLirItem =:= Item ->
	    pruning();
	true -> ok
    end.

pruning() ->
    NewQueue = lists:dropwhile(
		 fun(#cacheItem{} = X) ->
			 X#cacheItem.status /= lir end,
		 lists:reverse(lookup(lirQueue,2))),
    update(lirQueue,lists:reverse(NewQueue)).
    

%% 访问HIR资源。
access_hir(ID) ->
    case lists:filter(
	   fun(#cacheItem{} = X) ->
		   X#cacheItem.id == ID andalso 
		       X#cacheItem.status == hir end,
	   lookup(lirQueue,2)) of
	[Item] ->
	    in_queue(Item);
	[] ->
	    Item = update(ID,hir),
	    not_in_queue(Item)
    end.
    
not_in_queue(#cacheItem{} = Item) ->
    enter_queue(Item,lirQueue),
    move_top(Item,hirQueue).

in_queue(#cacheItem{} = Item) ->
    move_top(Item,lirQueue,lir),
    remove_from_queue(Item,hirQueue),
    old_lir_to_hir(),
    pruning().

old_lir_to_hir() ->
    OldestLirItem = lists:last(lookup(lirQueue,2)),
    remove_from_queue(OldestLirItem,lirQueue),
    NewHirItem = update(OldestLirItem#cacheItem.id,hir),
    enter_queue(NewHirItem,hirQueue).    

%% 访问缓存未命中资源。
access_non_resident(ID) ->
    ets:insert(visitTab,{newID,ID}), %%注入代码
    before_enter_hir_queue(),
    case lists:filter(
	   fun(#cacheItem{} = X) ->
		   X#cacheItem.id == ID andalso
		       X#cacheItem.status == non_resident
	   end,lookup(lirQueue,2)) of
	[Item] ->
	    in_queue(Item);
	[] ->
	    Item = update(ID,hir),
	    not_in_queue(Item)
    end.

visit_resource(ID,Flag) ->
    PrevStatus = get_res_prev_status(ID),
    CurLirNum = get_cur_lirs_num(),
    LirSize = get_lir_cache_size(),
    if
	CurLirNum < LirSize ->
	    initial_stage(ID);
	PrevStatus == lir ->
	    access_lir(ID);
	PrevStatus == hir ->
	    access_hir(ID);
	PrevStatus == non_resident ->
	    access_non_resident(ID)
    end,
    cache_notify(Flag,
		 ets:member(visitTab,newID),
		 ets:member(visitTab,oldID)).

%% 异步版本
cache_notify(Flag,false,false) when
      Flag == ?SYNC; Flag == ?ASYNC ->
    true;
cache_notify(?ASYNC,true,true) ->
    NewID = ets:lookup_element(visitTab,newID,2),
    OldID = ets:lookup_element(visitTab,oldID,2),
    cache:notify(NewID,OldID),
    ets:delete_all_objects(visitTab);
cache_notify(?ASYNC,true,false) ->
    NewID = ets:lookup_element(visitTab,newID,2),
    cache:notify(NewID),
    ets:delete_all_objects(visitTab);

%% 同步版本    
cache_notify(?SYNC,true,true) ->
    NewID = ets:lookup_element(visitTab,newID,2),
    OldID = ets:lookup_element(visitTab,oldID,2),
    ets:delete_all_objects(visitTab),
    {NewID,OldID};
cache_notify(?SYNC,true,false) ->
    NewID = ets:lookup_element(visitTab,newID,2),
    ets:delete_all_objects(visitTab),
    NewID.

reset_status() ->
    [{conf,{size,Size},{lirPercent,Percent}}] = lookup(conf),
    ets:delete_all_objects(lirsRam),
    ets:delete_all_objects(visitTab),
    init_tables(Size,Percent).

%% debug(ID) ->
%%     ?debugFmt("lirQueue is:~p~n",[lookup(lirQueue,2)]),
%%     ?debugFmt("hirQueue is:~p~n",[lookup(hirQueue,2)]),
%%     ?debugFmt("Current Res:~p~n",[lookup(ID)]),
%%     ?debugMsg("######################").
	
    


	    
	    
	
    
		
    
