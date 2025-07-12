%% -----------------------------------------------------------
%% Node Manager - monitors node connections
%% Implements basic node monitoring using net_kernel:monitor_nodes/1
%% -----------------------------------------------------------
-module(node_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, get_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_nodes() ->
    gen_server:call(?MODULE, get_nodes).

init([]) ->
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    {ok, #{nodes => nodes()}}.

handle_call(get_nodes, _From, State) ->
    {reply, maps:get(nodes, State), State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    io:format("Node up: ~p~n", [Node]),
    {noreply, State#{nodes => lists:usort([Node | maps:get(nodes, State)])}};
handle_info({nodedown, Node, _Info}, State) ->
    io:format("Node down: ~p~n", [Node]),
    {noreply, State#{nodes => lists:delete(Node, maps:get(nodes, State))}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
