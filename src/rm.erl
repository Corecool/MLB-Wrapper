%%%-------------------------------------------------------------------
%%% @author Corecool <>
%%% @copyright (C) 2011, Corecool
%%% @doc
%%%
%%% @end
%%% Created :  1 Jun 2011 by Corecool <>
%%%-------------------------------------------------------------------
-module(rm).

-behaviour(gen_server).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").
%% API
-export([start_link/0,stop/0]).
-export([find_res/1,update_res/1,remove_res/1,reload_res/0]).

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
%% Stop RM Server
%%
%% @spec stop() -> {stop,Reason,State}
%% @end
%%--------------------------------------------------------------------			
stop() ->
    gen_server:cast(?SERVER,stop).

%%--------------------------------------------------------------------
%% @doc
%% Find the resource
%%
%% @spec find_res(#resource{} | ID) -> #resource{} | notexist
%% @end
%%--------------------------------------------------------------------
find_res(#resource{} = Res) -> 
    gen_server:call(?SERVER,{find_res,Res},infinity);
find_res(ID) when is_integer(ID) ->
    gen_server:call(?SERVER,{find_res,ID},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Update the resource
%%
%% @spec update_res(#resource{}) -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
update_res(#resource{} = Res) ->
    gen_server:cast(?SERVER,{update_res,Res}).

%%--------------------------------------------------------------------
%% @doc
%% Remove the resource
%%
%% @spec remove_res(#resource{}) -> {noreply,State}
%% @end
%%--------------------------------------------------------------------
remove_res(#resource{} = Res) ->
    gen_server:cast(?SERVER,{remove_res,Res}).

%%--------------------------------------------------------------------
%% @doc
%% Reload the resource
%%
%% @spec reload_res() ->  {noreply,State}
%% @end
%%--------------------------------------------------------------------
reload_res() ->
    gen_server:cast(?SERVER,reload_res).
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
    Filename = "/tmp/" ++ pid_to_list(self()),
    dets:open_file(rmTab,[{auto_save,10000},
			  {keypos,#resource.id},
			  {file,Filename}]),
    case dets:info(rmTab,no_objects) of
	0 ->
	    reload_resource();
	_ ->
	    ok
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
handle_call({find_res,#resource{id = ID}},_From,State) ->
    {reply,find(ID),State};
handle_call({find_res,ID},_From,State) when 
      is_integer(ID) ->
    {reply,find(ID),State}.



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

handle_cast({update_res,#resource{} = Res},State) ->
    ok = dets:insert(rmTab,Res),
    {noreply,State};

handle_cast({remove_res,#resource{id = ID} = _Res},State) ->
    ok = dets:delete(rmTab,ID),
    {noreply,State};

handle_cast(reload_res,State) ->
    dets:delete_all_objects(rmTab),
    reload_resource(),
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

reload_resource() ->
    Seq = lists:seq(1,1000),
    lists:foreach(
      fun(X) ->
	      dets:insert(rmTab,
			  #resource{id = X}) end,
      Seq).

find(ID) ->
    case dets:lookup(rmTab,ID) of
	[Item] ->
	    Item;
	[] ->
	    notexist
    end.
     
    
