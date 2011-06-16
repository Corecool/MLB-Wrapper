%%%-------------------------------------------------------------------
%%% @author Corecool <>
%%% @copyright (C) 2011, Corecool
%%% @doc
%%%
%%% @end
%%% Created : 12 Jun 2011 by Corecool <>
%%%-------------------------------------------------------------------
-module(cache).

-behaviour(gen_server).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

%% API
-export([start_link/0,stop/0]).
-export([visit_res/1,notify/1,notify/2]).

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
%% Visit the cache to get the resource.
%%
%% @spec visit_res(#resource{}) -> #resource{} | notexist
%% @end
%%--------------------------------------------------------------------
visit_res(#resource{} = Res) ->
    gen_server:call(?SERVER,{visit,Res},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Notify the cache to update.
%%
%% @spec notify() -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
notify(NewID) when is_integer(NewID) ->
    gen_server:cast(?SERVER,{cacheNotify,NewID}).

notify(NewID,OldID) when 
      is_integer(NewID), is_integer(OldID)->
    gen_server:cast(?SERVER,{cacheNotify,NewID,OldID}).

    

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
init(_Args) ->
    ets:new(cacheTab,[named_table,{keypos,#resource.id}]),
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

%% 异步版本
handle_call({visit,#resource{id = ID} = Res},_From,State) ->
    case ets:member(cacheTab,ID) of
	true ->
	    lirs:visit_res(ID),
	    [Reply] = ets:lookup(cacheTab,ID);
	false ->
	    Reply = rm:find_res(Res),
	    if
		Reply /= notexist ->
		    lirs:visit_res(ID);
		true -> ok
	    end
    end,
    {reply,Reply,State}.

%% 同步版本
%% handle_call({visit,#resource{id = ID} = Res},_From,State) ->
%%     case ets:member(cacheTab,ID) of
%% 	true ->
%% 	    case lirs:visit_res(ID) of
%% 		{NewID,OldID} ->
%% 		    remove_res(OldID),
%% 		    add_res(NewID);
%% 		NewID when is_integer(NewID) ->
%% 		    add_res(NewID);
%% 		true -> ok
%% 	    end,
%% 	    [Reply] = ets:lookup(cacheTab,ID);
%% 	false ->
%% 	    Reply = rm:find_res(Res),
%% 	    if
%% 		Reply /= notexist ->
%% 		    case lirs:visit_res(ID) of
%% 			{NewID,OldID} ->
%% 			    remove_res(OldID),
%% 			    add_res(NewID);
%% 			NewID when is_integer(NewID) ->
%% 			    add_res(NewID);
%% 			true -> ok
%% 		    end;
%% 		true -> ok
%% 	    end
%%     end,
%%     {reply,Reply,State}.
	  

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

%% 用于LIRS判断缓存尚未满时
handle_cast({cacheNotify,NewID},State) ->
    add_res(NewID),
    {noreply,State};
			  
%% 用于LIRS常规缓存替换，NewID为当前访问资源而OldID为置换出
%% 缓存的资源
handle_cast({cacheNotify,NewID,OldID},State) ->
    remove_res(OldID),
    add_res(NewID),
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
add_res(ID) ->
    case rm:find_res(ID) of
	notexist ->
	    true;
	Res ->
	    ets:insert(cacheTab,Res)
    end.

remove_res(ID) ->
    ets:delete(cacheTab,ID).
    
    
	    
