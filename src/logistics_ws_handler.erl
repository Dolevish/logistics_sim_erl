%% -----------------------------------------------------------
%% מודול מטפל WebSocket - מתוקן לעבוד עם מקור אמת יחיד
%% מנהל את החיבורים והתקשורת עם הדפדפן
%% שולח עדכונים בזמן אמת על מצב המערכת
%% -----------------------------------------------------------
-module(logistics_ws_handler).
-behaviour(cowboy_websocket).

%% Cowboy WebSocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% -----------------------------------------------------------
%% Cowboy WebSocket Callbacks
%% -----------------------------------------------------------

%% אתחול החיבור - מעבר לפרוטוקול WebSocket
init(Req, State) ->
    io:format("WebSocket connection initiated from ~p~n", [cowboy_req:peer(Req)]),
    {cowboy_websocket, Req, State}.

%% אתחול ה-WebSocket handler
websocket_init(State) ->
    io:format("WebSocket connection established~n"),
    
    %% רישום לקבלת עדכונים מ-state collector
    logistics_state_collector:subscribe(self()),
    
    %% שינוי: שליחת המצב המלא ממקור האמת היחיד
    send_full_state_to_client(),
    
    %% תזמון שליחת heartbeat כל 30 שניות
    erlang:send_after(30000, self(), heartbeat),
    
    {ok, State}.

%% טיפול בהודעות מהלקוח (דפדפן)
websocket_handle({text, Msg}, State) ->
    io:format("Received from client: ~p~n", [Msg]),
    
    %% פענוח ההודעה מ-JSON
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"ping">>} ->
            %% תגובה ל-ping
            Response = jsx:encode(#{type => <<"pong">>, timestamp => erlang:system_time(second)}),
            {reply, {text, Response}, State};
            
        #{<<"type">> := <<"request_full_state">>} ->
            %% שינוי: הלקוח מבקש את כל המצב ממקור האמת
            send_full_state_to_client(),
            {ok, State};
            
        #{<<"type">> := <<"command">>, <<"action">> := Action} ->
            %% טיפול בפקודות מהלקוח
            handle_client_command(Action),
            {ok, State};
            
        _ ->
            io:format("Unknown message type: ~p~n", [Msg]),
            {ok, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

%% טיפול בהודעות פנימיות של Erlang
websocket_info({state_update, UpdateType, Data}, State) ->
    %% קיבלנו עדכון מ-state collector
    Update = #{
        type => <<"state_update">>,
        update_type => UpdateType,
        data => Data,
        timestamp => erlang:system_time(second)
    },
    
    %% שליחת העדכון ללקוח
    {reply, {text, jsx:encode(Update)}, State};

%% שינוי: טיפול בהודעה פנימית לשליחת מצב מלא
websocket_info({send_full_state, FullState}, State) ->
    Update = #{
        type => <<"state_update">>,
        update_type => <<"full_state">>,
        data => FullState,
        timestamp => erlang:system_time(second)
    },
    {reply, {text, jsx:encode(Update)}, State};

websocket_info(heartbeat, State) ->
    %% שליחת heartbeat ללקוח
    Heartbeat = jsx:encode(#{type => <<"heartbeat">>, timestamp => erlang:system_time(second)}),
    erlang:send_after(30000, self(), heartbeat),
    {reply, {text, Heartbeat}, State};

websocket_info(Info, State) ->
    io:format("WebSocket received info: ~p~n", [Info]),
    {ok, State}.

%% סיום החיבור
terminate(_Reason, _Req, _State) ->
    io:format("WebSocket connection closed~n"),
    %% ביטול הרישום לעדכונים
    logistics_state_collector:unsubscribe(self()),
    ok.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

%% שינוי: פונקציה חדשה ופשוטה לשליחת מצב מלא
send_full_state_to_client() ->
    io:format("WebSocket: Requesting full state from State Collector~n"),
    case logistics_state_collector:get_full_state() of
        {ok, FullState} ->
            %% שולח הודעה פנימית לתהליך הנוכחי כדי לשלוח את המידע ללקוח
            self() ! {send_full_state, FullState};
        {error, Reason} ->
            io:format("WebSocket: Failed to get full state: ~p~n", [Reason])
    end.

%% שינוי: הפונקציות הבאות הוסרו כי הן כבר לא נחוצות.
%% המידע מגיע ישירות מ-logistics_state_collector.
%% get_all_courier_states() -> ...
%% get_all_package_states() -> ...
%% get_all_zone_states() -> ...
%% get_system_stats() -> ...

%% טיפול בפקודות מהלקוח
handle_client_command(<<"pause_simulation">>) ->
    io:format("Client requested pause simulation~n"),
    control_center:pause_simulation();

handle_client_command(<<"resume_simulation">>) ->
    io:format("Client requested resume simulation~n"),
    control_center:resume_simulation();

handle_client_command(<<"emergency_stop">>) ->
    io:format("Client requested emergency stop~n"),
    control_center:emergency_stop();

handle_client_command(Action) ->
    io:format("Unknown client action: ~p~n", [Action]).
