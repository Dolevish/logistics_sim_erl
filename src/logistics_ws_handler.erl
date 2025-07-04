%% -----------------------------------------------------------
%% מודול מטפל WebSocket משופר - תומך בהגדרות דינמיות ומפה
%% מנהל את החיבורים והתקשורת עם הדפדפן
%% שולח עדכונים בזמן אמת על מצב המערכת כולל מיקומי שליחים
%% -----------------------------------------------------------
-module(logistics_ws_handler).
-behaviour(cowboy_websocket).

%% Cowboy WebSocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% הגדרת האזורים הקבועים
-define(FIXED_ZONES, ["north", "center", "south"]).

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
    logistics_state_collector:subscribe(self()),
    send_current_simulation_state(),
    erlang:send_after(30000, self(), heartbeat),
    {ok, State}.

%% טיפול בהודעות מהלקוח (דפדפן)
websocket_handle({text, Msg}, State) ->
    io:format("Received from client: ~p~n", [Msg]),
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"ping">>} ->
            Response = jsx:encode(#{type => <<"pong">>, timestamp => erlang:system_time(second)}),
            {reply, {text, Response}, State};
        #{<<"type">> := <<"pong">>} ->
            {ok, State};
        #{<<"type">> := <<"request_simulation_state">>} ->
            send_current_simulation_state(),
            {ok, State};
        #{<<"type">> := <<"request_full_state">>} ->
            send_full_state_to_client(),
            {ok, State};
        #{<<"type">> := <<"command">>, <<"action">> := Action} = Cmd ->
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
    Update = #{ type => <<"state_update">>, update_type => UpdateType, data => Data, timestamp => erlang:system_time(second) },
    {reply, {text, jsx:encode(Update)}, State};
websocket_info({simulation_state_update, SimState, Config}, State) ->
    Update = #{ type => <<"simulation_state">>, state => atom_to_binary(SimState, utf8), config => Config, timestamp => erlang:system_time(second) },
    {reply, {text, jsx:encode(Update)}, State};
websocket_info({send_full_state, FullState}, State) ->
    Update = #{ type => <<"state_update">>, update_type => <<"full_state">>, data => FullState, timestamp => erlang:system_time(second) },
    {reply, {text, jsx:encode(Update)}, State};
websocket_info({text, Message}, State) ->
    {reply, {text, Message}, State};
websocket_info(heartbeat, State) ->
    Heartbeat = jsx:encode(#{type => <<"heartbeat">>, timestamp => erlang:system_time(second)}),
    erlang:send_after(30000, self(), heartbeat),
    {reply, {text, Heartbeat}, State};
websocket_info(Info, State) ->
    io:format("WebSocket received info: ~p~n", [Info]),
    {ok, State}.

%% סיום החיבור
terminate(_Reason, _Req, _State) ->
    io:format("WebSocket connection closed~n"),
    logistics_state_collector:unsubscribe(self()),
    ok.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

send_current_simulation_state() ->
    case control_center:get_status() of
        {SimState, StateData} ->
            Config = maps:get(simulation_config, StateData, #{}),
            self() ! {simulation_state_update, SimState, Config};
        _ ->
            self() ! {simulation_state_update, idle, #{}}
    end.

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
    Config = extract_config_from_command(Cmd),
    case control_center:start_simulation(Config) of
        {ok, Message} -> send_command_response(<<"start_simulation">>, true, Message);
        {error, Reason} -> send_command_response(<<"start_simulation">>, false, list_to_binary(io_lib:format("~p", [Reason])))
    end;
handle_client_command(<<"stop_simulation">>, _Cmd) ->
    io:format("Client requested stop simulation~n"),
    case control_center:stop_simulation() of
        {ok, Message} -> send_command_response(<<"stop_simulation">>, true, Message);
        {error, Reason} -> send_command_response(<<"stop_simulation">>, false, list_to_binary(io_lib:format("~p", [Reason])))
    end;
handle_client_command(<<"pause_simulation">>, _Cmd) ->
    control_center:pause_simulation(),
    send_command_response(<<"pause_simulation">>, true, <<"Simulation paused">>);
handle_client_command(<<"continue_simulation">>, _Cmd) ->
    control_center:continue_simulation(),
    send_command_response(<<"continue_simulation">>, true, <<"Simulation continued">>);
handle_client_command(<<"pause_order_generator">>, _Cmd) ->
    control_center:pause_order_generator(),
    send_command_response(<<"pause_order_generator">>, true, <<"Order generator paused">>);
handle_client_command(<<"continue_order_generator">>, _Cmd) ->
    control_center:continue_order_generator(),
    send_command_response(<<"continue_order_generator">>, true, <<"Order generator continued">>);
handle_client_command(<<"update_order_interval">>, Cmd) ->
    Interval = maps:get(<<"interval">>, Cmd),
    control_center:update_order_interval(Interval),
    send_command_response(<<"update_order_interval">>, true, <<"Order interval updated">>);
handle_client_command(<<"emergency_stop">>, _Cmd) ->
    control_center:emergency_stop(),
    send_command_response(<<"emergency_stop">>, true, <<"Emergency stop initiated">>);
handle_client_command(Action, _Cmd) ->
    io:format("Unknown client action: ~p~n", [Action]),
    send_command_response(Action, false, <<"Unknown command">>).

%% --- הערה חדשה: תיקון הלוגיקה של חילוץ ההגדרות ---
%% הפונקציה תוקנה כדי לקרוא נכון את מספר בתי האב מההודעה שנשלחה מהלקוח.
extract_config_from_command(Cmd) ->
    DefaultConfig = #{
        zones => ?FIXED_ZONES,
        num_couriers => 8,
        order_interval => 5000,
        min_travel_time => 10000,
        max_travel_time => 60000,
        enable_map => false,
        num_homes => 200
    },

    NumCouriers = maps:get(<<"num_couriers">>, Cmd, maps:get(num_couriers, DefaultConfig)),
    OrderInterval = maps:get(<<"order_interval">>, Cmd, maps:get(order_interval, DefaultConfig)),
    MinTravelTime = maps:get(<<"min_travel_time">>, Cmd, maps:get(min_travel_time, DefaultConfig)),
    MaxTravelTime = maps:get(<<"max_travel_time">>, Cmd, maps:get(max_travel_time, DefaultConfig)),

    EnableMap = maps:get(<<"enable_map">>, Cmd, true),

    NumHomes = if
        EnableMap ->
            % אם המפה מופעלת, חובה לקרוא את הערך מהלקוח.
            % אם הערך לא קיים ב-Cmd, נשתמש בברירת המחדל.
            maps:get(<<"num_homes">>, Cmd, maps:get(num_homes, DefaultConfig));
        true ->
            % אם המפה כבויה, אין בתים.
            0
    end,

    #{
        zones => ?FIXED_ZONES,
        num_couriers => NumCouriers,
        order_interval => OrderInterval,
        min_travel_time => MinTravelTime,
        max_travel_time => MaxTravelTime,
        enable_map => EnableMap,
        num_homes => NumHomes
    }.

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
    case whereis(logistics_state_collector) of
        undefined -> ok;
        _ ->
            logistics_state_collector:broadcast_message(jsx:encode(Response))
    end.
