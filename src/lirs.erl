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

%% API
-export([start_link/1]).
-export([visit_resource/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(cacheItem,{id,status}).

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
start_link(FileName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  FileName,[]).

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
init(FileName) ->
    create_tables(FileName),
    case dets:info(lirsDisk,size) of
	Size when Size > 0 ->
	    recovery_tables();
	Size when Size == 0 ->
	    init_tables()
    end,
	
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
handle_call(Request, _From, State) ->
    Reply = {Request,State},
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
handle_cast({visit,ID},State) ->
    visit_resource(ID),
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
    dets:close(lirsDisk),
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

create_tables(FileName) ->
    dets:open_file(lirsDisk,[{file,FileName}]),
    ets:new(lirsRam,[named_table]).
    
recovery_tables() ->
    ets:from_dets(lirsRam,lirsDisk).

init_tables() ->
    dets:insert(lirsDisk,{lirQueue,[]}),
    dets:insert(lirsDisk,{hirQueue,[]}),
    dets:insert(lirsDisk,{conf,{size,5},{lirPercent,0.6}}),
    ets:from_dets(lirsRam,lirsDisk).

lookup(Key) ->
    ets:lookup(lirsRam,Key).

lookup(Key,Pos) ->
    ets:lookup_element(lirsRam,Key,Pos).

update(ID,Status) when is_integer(ID) ->
    ets:insert(lirsRam,{ID,Status}),
    #cacheItem{id = ID,status = Status};
update(Key,Status) when is_atom(Key) ->
    %% dets:insert(lirsDisk,{Key,Value}),
    ets:insert(lirsRam,{Key,Status}).

%% 下面为业务逻辑方法，上面为持久化方法（涉及ets,dets）

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
    remove_from_queue(OldestHirItem,hirQueue).

is_full_hirqueue() ->
    HirQueueSize = get_total_cache_size() - 
	get_lir_cache_size(),
    length(lookup(hirQueue)) >= HirQueueSize.

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
	    pruning()
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
    remove_from_queue(OldestLirItem,lookup(lirQueue,2)),
    NewHirItem = update(OldestLirItem#cacheItem.id,hir),
    enter_queue(NewHirItem,hirQueue).    

%% 访问缓存未命中资源。
access_non_resident(ID) ->
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

visit_resource(ID) ->
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
    end.


	    
	    
	
    
		
    
