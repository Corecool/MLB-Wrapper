%%%-------------------------------------------------------------------
%%% @author Corecool <>
%%% @copyright (C) 2011, Corecool
%%% @doc
%%%
%%% @end
%%% Created : 18 Jun 2011 by Corecool <>
%%%-------------------------------------------------------------------
-module(monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,stop/0]).
-export([inc_cache_miss/0,inc_res_miss/0,inc_notify/0,
	 reset_counter/0]).
-export([get_cache_miss/0,get_res_miss/0,
	 get_notify_count/0]).

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
%% @spec stop() | stop(Pid) -> {stop,normal,State}
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER,stop).

%%--------------------------------------------------------------------
%% @doc
%% These functions are used to increase the monitor data.
%%
%% @spec inc_cache_miss() -> {noreply,State}
%% @spec inc_res_miss() -> {noreply,State}
%% @spec inc_notify() -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
inc_cache_miss() ->
    gen_server:cast(?SERVER,inc_cache).

inc_res_miss() ->
    gen_server:cast(?SERVER,inc_res).

inc_notify() ->
    gen_server:cast(?SERVER,inc_notify).

%%--------------------------------------------------------------------
%% @doc
%% These functions are used to get the monitor data.
%%
%% @spec get_cache_miss() -> Integer
%% @spec get_res_miss() -> Integer
%% @spec get_notify_count() -> Integer
%% @end
%%--------------------------------------------------------------------
get_cache_miss() ->
    gen_server:call(?SERVER,cache_miss).

get_res_miss() ->
    gen_server:call(?SERVER,res_miss).

get_notify_count() ->
    gen_server:call(?SERVER,notify_count).
%%--------------------------------------------------------------------
%% @doc
%% It is used to reset all counters.
%%
%% @spec reset_counter() -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
reset_counter() ->
    gen_server:cast(?SERVER,reset).

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
    create_tables(),
    init_tables(),
    {ok, ok}.

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
handle_call(cache_miss, _From, State) ->
    {cacheMiss,Counter} = lookup(cacheMiss),
    {reply, Counter, State};

handle_call(res_miss, _From, State) ->
    {resMiss,Counter} = lookup(resMiss),
    {reply,Counter,State};

handle_call(notify_count, _From, State) ->
    {notify,Counter} = lookup(notify),
    {reply,Counter,State}.



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

handle_cast(inc_cache,State) ->
    {cacheMiss,Counter} = lookup(cacheMiss),
    update({cacheMiss,Counter + 1}),
    {noreply,State};

handle_cast(inc_res,State) ->
    {resMiss,Counter} = lookup(resMiss),
    update({resMiss,Counter + 1}),
    {noreply,State};

handle_cast(inc_notify,State) ->
    {notify,Counter} = lookup(notify),
    update({notify,Counter + 1}),
    {noreply,State};

handle_cast(reset,State) ->
    init_tables(),
    {noreply,State}.
    

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
create_tables() ->
    ets:new(monitorTab,[named_table]).

init_tables() ->
    ets:insert(monitorTab,{cacheMiss,0}),
    ets:insert(monitorTab,{resMiss,0}),
    ets:insert(monitorTab,{notify,0}).

%% 因为MonitorTab是set类型
lookup(Key) ->
    case ets:lookup(monitorTab,Key) of
	[Obj] ->
	    Obj;
	[] ->
	    notexist
    end.

update(Obj) ->
    ets:insert(monitorTab,Obj).
