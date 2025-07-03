%% -----------------------------------------------------------
%% מודול מטפל WebSocket משופר - תומך בהגדרות דינמיות
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
    
    %% שליחת מצב הסימולציה הנוכחי
    send_current_simulation_state(),
    
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
            
        #{<<"type">> := <<"request_simulation_state">>} ->
            %% הלקוח מבקש את מצב הסימולציה
            send_current_simulation_state(),
            {ok, State};
            
        #{<<"type">> := <<"request_full_state">>} ->
            %% הלקוח מבקש את כל המצב
            send_full_state_to_client(),
            {ok, State};
            
        #{<<"type">> := <<"command">>, <<"action">> := Action} = Cmd ->
            %% טיפול בפקודות מהלקוח
            handle_client_command(Action, Cmd),
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
    
    {reply, {text, jsx:encode(Update)}, State};

%% טיפול בעדכון מצב הסימולציה
websocket_info({simulation_state_update, SimState, Config}, State) ->
    Update = #{
        type => <<"simulation_state">>,
        state => atom_to_binary(SimState, utf8),
        config => Config,
        timestamp => erlang:system_time(second)
    },
    
    {reply, {text, jsx:encode(Update)}, State};

%% טיפול בהודעה פנימית לשליחת מצב מלא
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

%% שליחת מצב הסימולציה הנוכחי
send_current_simulation_state() ->
    %% קבלת מצב מה-control center
    case control_center:get_status() of
        {SimState, StateData} ->
            Config = maps:get(simulation_config, StateData, #{}),
            self() ! {simulation_state_update, SimState, Config};
        _ ->
            %% ברירת מחדל - idle
            self() ! {simulation_state_update, idle, #{}}
    end.

%% שליחת מצב מלא של המערכת
send_full_state_to_client() ->
    io:format("WebSocket: Requesting full state from State Collector~n"),
    case logistics_state_collector:get_full_state() of
        {ok, FullState} ->
            self() ! {send_full_state, FullState};
        {error, Reason} ->
            io:format("WebSocket: Failed to get full state: ~p~n", [Reason])
    end.

%% טיפול בפקודות מהלקוח
handle_client_command(<<"start_simulation">>, Cmd) ->
    io:format("Client requested start simulation~n"),
    
    %% חילוץ ההגדרות מהפקודה
    Config = extract_config_from_command(Cmd),
    
    %% שליחת הפקודה ל-control center
    case control_center:start_simulation(Config) of
        {ok, Message} ->
            send_command_response(<<"start_simulation">>, true, Message);
        {error, Reason} ->
            send_command_response(<<"start_simulation">>, false, 
                                list_to_binary(io_lib:format("~p", [Reason])))
    end;

handle_client_command(<<"stop_simulation">>, _Cmd) ->
    io:format("Client requested stop simulation~n"),
    case control_center:stop_simulation() of
        {ok, Message} ->
            send_command_response(<<"stop_simulation">>, true, Message);
        {error, Reason} ->
            send_command_response(<<"stop_simulation">>, false, 
                                list_to_binary(io_lib:format("~p", [Reason])))
    end;

handle_client_command(<<"pause_simulation">>, _Cmd) ->
    io:format("Client requested pause simulation~n"),
    control_center:pause_simulation(),
    send_command_response(<<"pause_simulation">>, true, <<"Simulation paused">>);

handle_client_command(<<"resume_simulation">>, _Cmd) ->
    io:format("Client requested resume simulation~n"),
    control_center:resume_simulation(),
    send_command_response(<<"resume_simulation">>, true, <<"Simulation resumed">>);

handle_client_command(<<"emergency_stop">>, _Cmd) ->
    io:format("Client requested emergency stop~n"),
    control_center:emergency_stop(),
    send_command_response(<<"emergency_stop">>, true, <<"Emergency stop initiated">>);

handle_client_command(Action, _Cmd) ->
    io:format("Unknown client action: ~p~n", [Action]),
    send_command_response(Action, false, <<"Unknown command">>).

%% חילוץ הגדרות מפקודת הלקוח
extract_config_from_command(Cmd) ->
    %% ברירות מחדל
    DefaultConfig = #{
        zones => ["north", "center", "south"],
        num_couriers => 8,
        order_interval => 5000,
        min_travel_time => 10000,
        max_travel_time => 60000,
        simulation_speed => 1.0
    },
    
    %% חילוץ ערכים מהפקודה
    Zones = case maps:get(<<"zones">>, Cmd, undefined) of
        undefined -> maps:get(zones, DefaultConfig);
        ZoneList when is_list(ZoneList) -> 
            [case Z of
                Bin when is_binary(Bin) -> binary_to_list(Bin);
                List when is_list(List) -> List
            end || Z <- ZoneList];
        _ -> maps:get(zones, DefaultConfig)
    end,
    
    NumCouriers = case maps:get(<<"num_couriers">>, Cmd, undefined) of
        undefined -> maps:get(num_couriers, DefaultConfig);
        N when is_integer(N) -> N;
        _ -> maps:get(num_couriers, DefaultConfig)
    end,
    
    OrderInterval = case maps:get(<<"order_interval">>, Cmd, undefined) of
        undefined -> maps:get(order_interval, DefaultConfig);
        I when is_integer(I) -> I;
        _ -> maps:get(order_interval, DefaultConfig)
    end,
    
    MinTravelTime = case maps:get(<<"min_travel_time">>, Cmd, undefined) of
        undefined -> maps:get(min_travel_time, DefaultConfig);
        T when is_integer(T) -> T;
        _ -> maps:get(min_travel_time, DefaultConfig)
    end,
    
    MaxTravelTime = case maps:get(<<"max_travel_time">>, Cmd, undefined) of
        undefined -> maps:get(max_travel_time, DefaultConfig);
        T2 when is_integer(T2) -> T2;
        _ -> maps:get(max_travel_time, DefaultConfig)
    end,
    
    SimSpeed = case maps:get(<<"simulation_speed">>, Cmd, undefined) of
        undefined -> maps:get(simulation_speed, DefaultConfig);
        S when is_number(S) -> S;
        _ -> maps:get(simulation_speed, DefaultConfig)
    end,
    
    %% החזרת המפה המעודכנת
    #{
        zones => Zones,
        num_couriers => NumCouriers,
        order_interval => OrderInterval,
        min_travel_time => MinTravelTime,
        max_travel_time => MaxTravelTime,
        simulation_speed => SimSpeed
    }.

%% שליחת תגובה לפקודה
send_command_response(Command, Success, Message) ->
    Response = #{
        type => <<"command_response">>,
        command => Command,
        success => Success,
        message => case Message of
            M when is_binary(M) -> M;
            M when is_list(M) -> list_to_binary(M);
            _ -> <<"">>
        end,
        timestamp => erlang:system_time(second)
    },
    
    %% שליחה לכל המנויים דרך State Collector
    case whereis(logistics_state_collector) of
        undefined -> ok;
        _ -> 
            %% שליחת התגובה דרך State Collector שישדר לכל המנויים
            logistics_state_collector:broadcast_message(jsx:encode(Response))
    end.