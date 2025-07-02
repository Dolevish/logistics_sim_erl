%% -----------------------------------------------------------
%% מודול מטפל WebSocket - מתוקן לדיווח נכון על total_delivered
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
    
    %% שליחת מצב ראשוני
    send_initial_state(),
    
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
            %% הלקוח מבקש את כל המצב
            send_full_state(),
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

%% שליחת המצב הראשוני ללקוח
send_initial_state() ->
    io:format("Sending initial state to client~n"),
    
    %% שליחת מידע על כל השליחים
    CourierStates = get_all_courier_states(),
    self() ! {state_update, <<"couriers_init">>, CourierStates},
    
    %% שליחת מידע על כל החבילות
    PackageStates = get_all_package_states(),
    self() ! {state_update, <<"packages_init">>, PackageStates},
    
    %% שליחת סטטיסטיקות כלליות
    Stats = get_system_stats(),
    self() ! {state_update, <<"stats_init">>, Stats}.

%% שליחת המצב המלא
send_full_state() ->
    FullState = #{
        couriers => get_all_courier_states(),
        packages => get_all_package_states(),
        zones => get_all_zone_states(),
        stats => get_system_stats()
    },
    
    self() ! {state_update, <<"full_state">>, FullState}.

%% קבלת מצב כל השליחים - מתוקן לכלול total_delivered אמיתי
get_all_courier_states() ->
    %% רשימת כל השליחים במערכת
    CourierIds = ["courier1", "courier2", "courier3", "courier4", 
                  "courier5", "courier6", "courier7", "courier8"],
    
    lists:map(fun(CourierId) ->
        %% בדיקה אם התהליך קיים
        case whereis(list_to_atom("courier_" ++ CourierId)) of
            undefined ->
                #{id => list_to_binary(CourierId), 
                  status => <<"offline">>,
                  current_package => null,
                  delivered_packages => [],
                  total_delivered => 0};
            _Pid ->
                %% נסה לקבל מידע מ-State Collector
                case logistics_state_collector:get_courier_info(CourierId) of
                    {ok, Info} ->
                        %% יש מידע - השתמש בו
                        Info;
                    {error, not_found} ->
                        %% אין מידע - מחזיר ברירת מחדל
                        #{id => list_to_binary(CourierId),
                          status => <<"idle">>,
                          current_package => null,
                          delivered_packages => [],
                          total_delivered => 0}
                end
        end
    end, CourierIds).

%% קבלת מצב כל החבילות - מתוקן לכלול חבילות אמיתיות
get_all_package_states() ->
    %% אוסף חבילות מכל האזורים
    Zones = ["north", "center", "south"],
    AllPackages = lists:foldl(fun(Zone, Acc) ->
        %% נסה לקבל חבילות ממנהל האזור
        case whereis(list_to_atom("zone_manager_" ++ Zone)) of
            undefined -> 
                Acc;
            ZonePid ->
                try
                    %% קבל סטטיסטיקות מהאזור
                    case gen_statem:call(ZonePid, get_stats, 1000) of
                        #{waiting_packages := WaitingList} when is_list(WaitingList) ->
                            %% המר חבילות ממתינות לפורמט הנכון
                            WaitingPackages = lists:map(fun(PkgId) ->
                                #{
                                    id => list_to_binary(PkgId),
                                    status => <<"ordered">>,
                                    zone => list_to_binary(Zone),
                                    courier => null,
                                    created_at => erlang:system_time(second)
                                }
                            end, WaitingList),
                            Acc ++ WaitingPackages;
                        _ ->
                            Acc
                    end
                catch
                    _:_ -> Acc
                end
        end
    end, [], Zones),
    
    %% בדוק גם חבילות מ-State Collector אם קיימות
    StateCollectorPackages = case ets:info(package_states) of
        undefined -> [];
        _ ->
            try
                ets:foldl(fun({_Id, PackageInfo}, Acc) ->
                    [PackageInfo | Acc]
                end, [], package_states)
            catch
                _:_ -> []
            end
    end,
    
    %% מיזוג הרשימות ללא כפילויות
    CombinedPackages = AllPackages ++ StateCollectorPackages,
    
    %% הסרת כפילויות לפי ID
    UniquePackages = lists:foldl(fun(Package, Acc) ->
        PackageId = maps:get(id, Package),
        case lists:any(fun(P) -> maps:get(id, P) == PackageId end, Acc) of
            true -> Acc;  %% כבר קיים
            false -> [Package | Acc]  %% חדש
        end
    end, [], CombinedPackages),
    
    io:format("WebSocket: Found ~p packages total~n", [length(UniquePackages)]),
    UniquePackages.

%% קבלת מצב כל האזורים
get_all_zone_states() ->
    Zones = ["north", "center", "south"],
    lists:map(fun(Zone) ->
        %% נסה לקבל מידע מ-State Collector
        case logistics_state_collector:get_zone_info(Zone) of
            {ok, Info} ->
                Info;
            {error, not_found} ->
                %% ברירת מחדל
                #{zone => list_to_binary(Zone),
                  waiting_packages => 0,
                  active_deliveries => 0,
                  total_delivered => 0}
        end
    end, Zones).

%% קבלת סטטיסטיקות המערכת
get_system_stats() ->
    %% TODO: לקרוא לסטטיסטיקות אמיתיות
    #{total_packages => 0,
      delivered_packages => 0,
      failed_packages => 0,
      average_delivery_time => 0,
      system_uptime => erlang:system_time(second)}.

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