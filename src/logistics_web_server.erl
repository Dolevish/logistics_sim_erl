%% -----------------------------------------------------------
%% מודול שרת ווב (Web Server)
%% מפעיל שרת HTTP עם Cowboy ומטפל ב-WebSocket connections
%% משמש כנקודת כניסה לממשק הגרפי של המערכת
%% -----------------------------------------------------------
-module(logistics_web_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% הגדרת פורט ברירת מחדל
-define(DEFAULT_PORT, 8080).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

%% התחלת שרת הווב
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% עצירת שרת הווב
stop() ->
    gen_server:call(?MODULE, stop).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Starting Logistics Web Server on port ~p...~n", [?DEFAULT_PORT]),
    
    %% הגדרת נתיבי הראוטינג
    Dispatch = cowboy_router:compile([
        {'_', [
            %% WebSocket endpoint
            {"/websocket", logistics_ws_handler, []},
            %% Static files - מציג קבצים סטטיים מתיקיית priv/static
            {"/", cowboy_static, {priv_file, logistics_sim, "static/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, logistics_sim, "static"}}
        ]}
    ]),
    
    %% התחלת שרת Cowboy
    case cowboy:start_clear(logistics_http_listener,
                           [{port, ?DEFAULT_PORT}],
                           #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            io:format("Web server started successfully at http://localhost:~p~n", [?DEFAULT_PORT]),
            {ok, #{port => ?DEFAULT_PORT, listener => logistics_http_listener}};
        {error, Reason} ->
            io:format("Failed to start web server: ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    io:format("Stopping Logistics Web Server...~n"),
    cowboy:stop_listener(maps:get(listener, State)),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("Logistics Web Server terminating...~n"),
    %% וודא שה-listener נעצר
    case maps:get(listener, State, undefined) of
        undefined -> ok;
        Listener -> cowboy:stop_listener(Listener)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.