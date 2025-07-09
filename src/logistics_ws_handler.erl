%% -----------------------------------------------------------
%% מודול מטפל WebSocket משופר - תומך בהגדרות דינמיות ומפה
%% מנהל את החיבורים והתקשורת עם הדפדפן
%% שולח עדכונים בזמן אמת על מצב המערכת כולל מיקומי שליחים
%% -- גרסה מתוקנת עם טיפול מלא בפקודות --
%% -----------------------------------------------------------
-module(logistics_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(FIXED_ZONES, ["north", "center", "south"]).

%% הערה: אתחול החיבור - מעבר לפרוטוקול WebSocket
init(Req, State) ->
    io:format("WebSocket connection initiated from ~p~n", [cowboy_req:peer(Req)]),
    {cowboy_websocket, Req, State}.

%% הערה: הרשמה לעדכונים עם פתיחת החיבור
websocket_init(State) ->
    io:format("WebSocket connection established~n"),
    logistics_state_collector:subscribe(self()),
    send_current_simulation_state(),
    erlang:send_after(30000, self(), heartbeat),
    {ok, State}.

%% הערה: טיפול בהודעות טקסט מהלקוח
websocket_handle({text, Msg}, State) ->
    io:format("WS Handler: Received from client: ~p~n", [Msg]),
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"ping">>} ->
            {reply, {text, jsx:encode(#{type => <<"pong">>})}, State};
        #{<<"type">> := <<"pong">>} ->
            {ok, State};
        #{<<"type">> := <<"request_simulation_state">>} ->
            send_current_simulation_state(),
            {ok, State};
        %% *** התיקון מתחיל כאן ***
        #{<<"type">> := <<"command">>, <<"action">> := <<"request_full_state">>} ->
            send_full_state_to_client(),
            {ok, State};
        %% *** התיקון מסתיים כאן ***
        #{<<"type">> := <<"command">>, <<"action">> := Action} = Cmd ->
            handle_client_command(Action, Cmd),
            {ok, State};
        _ ->
            io:format("WS Handler: Unknown message type: ~p~n", [Msg]),
            {ok, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

%% הערה: טיפול בהודעות פנימיות מהמערכת
websocket_info({state_update, UpdateType, Data}, State) ->
    Update = #{type => <<"state_update">>, update_type => UpdateType, data => Data},
    {reply, {text, jsx:encode(Update)}, State};
websocket_info({simulation_state_update, SimState, Config}, State) ->
    Update = #{type => <<"simulation_state">>, state => atom_to_binary(SimState, utf8), config => Config},
    {reply, {text, jsx:encode(Update)}, State};
websocket_info({send_full_state, FullState}, State) ->
    Update = #{type => <<"state_update">>, update_type => <<"full_state">>, data => FullState},
    {reply, {text, jsx:encode(Update)}, State};
websocket_info({text, Message}, State) ->
    {reply, {text, Message}, State};
websocket_info(heartbeat, State) ->
    erlang:send_after(30000, self(), heartbeat),
    {reply, {text, jsx:encode(#{type => <<"heartbeat">>})}, State};
websocket_info(Info, State) ->
    io:format("WS Handler: Received unhandled info: ~p~n", [Info]),
    {ok, State}.

%% הערה: ניתוק מהעדכונים בסגירת החיבור
terminate(_Reason, _Req, _State) ->
    io:format("WebSocket connection closed~n"),
    logistics_state_collector:unsubscribe(self()),
    ok.

%% --- פונקציות עזר פרטיות ---

send_current_simulation_state() ->
    case control_center:get_status() of
        {SimState, StateData} ->
            self() ! {simulation_state_update, SimState, maps:get(simulation_config, StateData, #{})};
        _ ->
            self() ! {simulation_state_update, idle, #{}}
    end.
    
send_full_state_to_client() ->
    case logistics_state_collector:get_full_state() of
        {ok, FullState} -> self() ! {send_full_state, FullState};
        {error, Reason} -> io:format("WebSocket: Failed to get full state: ~p~n", [Reason])
    end.
    
%% הערה: פונקציה מרכזית לטיפול בפקודות מהלקוח. כל הפקודות שהיו חסרות הוחזרו.
handle_client_command(<<"start_simulation">>, Cmd) ->
    Config = extract_config_from_command(Cmd),
    control_center:start_simulation(Config);
handle_client_command(<<"stop_simulation">>, _Cmd) ->
    control_center:stop_simulation();
handle_client_command(<<"pause_simulation">>, _Cmd) ->
    control_center:pause_simulation();
handle_client_command(<<"continue_simulation">>, _Cmd) ->
    control_center:continue_simulation();
handle_client_command(<<"pause_order_generator">>, _Cmd) ->
    control_center:pause_order_generator();
handle_client_command(<<"continue_order_generator">>, _Cmd) ->
    control_center:continue_order_generator();
handle_client_command(<<"update_order_interval">>, Cmd) ->
    Interval = maps:get(<<"interval">>, Cmd),
    control_center:update_order_interval(Interval);
handle_client_command(Action, _Cmd) ->
    io:format("WS Handler: Unknown client action: ~p~n", [Action]).

%% הערה: חילוץ הגדרות מהפקודה שנשלחה מהלקוח.
extract_config_from_command(Cmd) ->
    DefaultConfig = #{ num_couriers => 8, order_interval => 5000 },
    NumCouriers = maps:get(<<"num_couriers">>, Cmd, maps:get(num_couriers, DefaultConfig)),
    OrderInterval = maps:get(<<"order_interval">>, Cmd, maps:get(order_interval, DefaultConfig)),
    #{ zones => ?FIXED_ZONES, num_couriers => NumCouriers, order_interval => OrderInterval, enable_map => true }.