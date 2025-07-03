-module(control_center).
-behaviour(gen_statem).

-export([start_link/0, start_simulation/1, stop_simulation/0, pause_simulation/0, resume_simulation/0, get_status/0, emergency_stop/0]).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([idle/3, initializing/3, running/3, paused/3, degraded/3, recovering/3, shutting_down/3, halted/3]).

%% -----------------------------------------------------------
%% API Functions - פונקציות ציבוריות לשליטה במרכז הבקרה
%% -----------------------------------------------------------

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() ->
    state_functions.

%% התחלת סימולציה עם הגדרות
start_simulation(Config) ->
    gen_statem:call(?MODULE, {start_simulation, Config}).

%% עצירת הסימולציה וחזרה למצב idle
stop_simulation() ->
    gen_statem:call(?MODULE, stop_simulation).

%% השהיית הסימולציה
pause_simulation() ->
    gen_statem:cast(?MODULE, {pause}).

%% המשך הסימולציה
resume_simulation() ->
    gen_statem:cast(?MODULE, {resume}).

%% קבלת סטטוס מרכז הבקרה
get_status() ->
    gen_statem:call(?MODULE, get_status).

%% עצירת חירום של המערכת
emergency_stop() ->
    gen_statem:cast(?MODULE, {shutdown_requested}).

%% -----------------------------------------------------------
%% gen_statem callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Control Center initializing in idle mode...~n"),
    {ok, idle, #{
        simulation_config => #{},
        simulation_sup => undefined,
        zones => [],
        healthy_zones => [],
        failed_zones => [],
        start_time => undefined
    }}.

%% -----------------------------------------------------------
%% States - מצבי ה-FSM השונים
%% -----------------------------------------------------------

%% מצב idle - ממתין להגדרות והפעלת סימולציה
idle({call, From}, {start_simulation, Config}, State) ->
    io:format("Starting simulation with config: ~p~n", [Config]),
    
    %% אתחול הסופרווייזר הדינמי
    case start_simulation_supervisor() of
        {ok, SupPid} ->
            %% התחלת רכיבי הסימולציה
            case start_simulation_components(SupPid, Config) of
                ok ->
                    %% עדכון המצב עם ההגדרות
                    Zones = maps:get(zones, Config, ["north", "center", "south"]),
                    NewState = State#{
                        simulation_config => Config,
                        simulation_sup => SupPid,
                        zones => Zones,
                        start_time => erlang:system_time(second)
                    },
                    
                    %% מעבר למצב אתחול
                    gen_statem:reply(From, {ok, "Simulation starting"}),
                    
                    %% תזמון בדיקת תקינות ראשונית
                    erlang:send_after(2000, self(), {check_zones_health}),
                    
                    {next_state, initializing, NewState};
                {error, Reason} ->
                    gen_statem:reply(From, {error, Reason}),
                    {keep_state, State}
            end;
        {error, Reason} ->
            gen_statem:reply(From, {error, Reason}),
            {keep_state, State}
    end;

idle({call, From}, get_status, State) ->
    {keep_state, State, [{reply, From, {idle, State}}]};

idle(EventType, EventContent, State) ->
    io:format("Idle received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב אתחול - בודק שכל האזורים פעילים
initializing(cast, {system_start}, State) ->
    io:format("System start received, checking all zones...~n"),
    case check_all_zones_ready(State) of
        true ->
            io:format("All zones ready -> Transitioning to Running~n"),
            erlang:send_after(10000, self(), {check_zones_health}),
            
            %% דיווח למערכת על מצב הסימולציה
            report_simulation_state(running, maps:get(simulation_config, State)),
            
            {next_state, running, State#{healthy_zones => maps:get(zones, State)}};
        false ->
            io:format("Not all zones ready, retrying in 2 seconds...~n"),
            erlang:send_after(2000, self(), {retry_system_start}),
            {keep_state, State}
    end;

initializing(info, {check_zones_health}, State) ->
    io:format("Checking zones health during initialization...~n"),
    case check_all_zones_ready(State) of
        true ->
            io:format("All zones are healthy -> Starting simulation~n"),
            erlang:send_after(10000, self(), {check_zones_health}),
            
            %% דיווח למערכת על מצב הסימולציה
            report_simulation_state(running, maps:get(simulation_config, State)),
            
            {next_state, running, State#{healthy_zones => maps:get(zones, State)}};
        false ->
            erlang:send_after(5000, self(), {check_zones_health}),
            {keep_state, State}
    end;

initializing(info, {retry_system_start}, State) ->
    gen_statem:cast(self(), {system_start}),
    {keep_state, State};

initializing({call, From}, stop_simulation, State) ->
    stop_all_simulation_components(State),
    gen_statem:reply(From, {ok, "Simulation stopped"}),
    report_simulation_state(idle, #{}),
    {next_state, idle, reset_state()};

initializing(EventType, EventContent, State) ->
    io:format("Initializing received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב ריצה רגיל - המערכת פועלת תקין
running(cast, {pause}, State) ->
    io:format("Simulation paused by operator~n"),
    pause_all_order_generators(State),
    report_simulation_state(paused, maps:get(simulation_config, State)),
    {next_state, paused, State};

running(cast, {zone_failure_detected, Zone}, State) ->
    io:format("Zone failure detected: ~p -> Entering degraded mode~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    handle_zone_failure(Zone),
    {next_state, degraded, NewState};

running(info, {check_zones_health}, State) ->
    FailedZones = check_zones_health(State),
    case FailedZones of
        [] ->
            erlang:send_after(10000, self(), {check_zones_health}),
            {keep_state, State};
        Failed ->
            io:format("Zones failed health check: ~p~n", [Failed]),
            gen_statem:cast(self(), {zone_failure_detected, hd(Failed)}),
            {keep_state, State}
    end;

running(cast, {shutdown_requested}, State) ->
    io:format("Shutdown requested -> Starting graceful shutdown~n"),
    initiate_graceful_shutdown(State),
    {next_state, shutting_down, State};

running({call, From}, stop_simulation, State) ->
    stop_all_simulation_components(State),
    gen_statem:reply(From, {ok, "Simulation stopped"}),
    report_simulation_state(idle, #{}),
    {next_state, idle, reset_state()};

running({call, From}, get_status, State) ->
    {keep_state, State, [{reply, From, {running, State}}]};

running(EventType, EventContent, State) ->
    io:format("Running received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב השהיה - הסימולציה מושהית
paused(cast, {resume}, State) ->
    io:format("Simulation resumed by operator~n"),
    resume_all_order_generators(State),
    erlang:send_after(10000, self(), {check_zones_health}),
    report_simulation_state(running, maps:get(simulation_config, State)),
    {next_state, running, State};

paused(cast, {zone_failure_detected, Zone}, State) ->
    io:format("Zone failure detected while paused: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    {next_state, degraded, NewState};

paused({call, From}, stop_simulation, State) ->
    stop_all_simulation_components(State),
    gen_statem:reply(From, {ok, "Simulation stopped"}),
    report_simulation_state(idle, #{}),
    {next_state, idle, reset_state()};

paused({call, From}, get_status, State) ->
    {keep_state, State, [{reply, From, {paused, State}}]};

paused(EventType, EventContent, State) ->
    io:format("Paused received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% המצבים הנוספים (degraded, recovering, shutting_down, halted) נשארים דומים...
degraded(cast, {zone_recovered, Zone}, State) ->
    io:format("Zone recovered: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => [Zone | HealthyZones],
        failed_zones => lists:delete(Zone, FailedZones)
    },
    case maps:get(failed_zones, NewState) of
        [] ->
            io:format("All zones recovered -> Returning to normal operation~n"),
            erlang:send_after(10000, self(), {check_zones_health}),
            {next_state, running, NewState};
        _ ->
            io:format("Still have failed zones: ~p~n", [maps:get(failed_zones, NewState)]),
            {keep_state, NewState}
    end;

degraded(cast, {attempt_recovery}, State) ->
    io:format("Attempting to recover failed zones~n"),
    {next_state, recovering, State};

degraded({call, From}, get_status, State) ->
    {keep_state, State, [{reply, From, {degraded, State}}]};

degraded(EventType, EventContent, State) ->
    io:format("Degraded received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

recovering(cast, {zone_recovered, Zone}, State) ->
    io:format("Zone recovered during recovery: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => [Zone | HealthyZones],
        failed_zones => lists:delete(Zone, FailedZones)
    },
    case maps:get(failed_zones, NewState) of
        [] ->
            io:format("All zones recovered successfully -> Returning to running~n"),
            erlang:send_after(10000, self(), {check_zones_health}),
            {next_state, running, NewState};
        _ ->
            {keep_state, NewState}
    end;

recovering(info, {recovery_timeout}, State) ->
    io:format("Recovery timeout -> Returning to degraded mode~n"),
    {next_state, degraded, State};

recovering(EventType, EventContent, State) ->
    io:format("Recovering received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

shutting_down(info, {shutdown_complete}, State) ->
    io:format("All systems shutdown complete~n"),
    {next_state, halted, State};

shutting_down(cast, {zone_shutdown_complete, Zone}, State) ->
    io:format("Zone ~p shutdown complete~n", [Zone]),
    case all_zones_shutdown(State) of
        true ->
            erlang:send_after(100, self(), {shutdown_complete}),
            {keep_state, State};
        false ->
            {keep_state, State}
    end;

shutting_down(EventType, EventContent, State) ->
    io:format("Shutting down received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

halted(EventType, EventContent, State) ->
    io:format("System halted, ignoring event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות - Dynamic Simulation Management
%% -----------------------------------------------------------

%% התחלת סופרווייזר דינמי לסימולציה
start_simulation_supervisor() ->
    %% יצירת סופרווייזר דינמי תחת הסופרווייזר הראשי
    ChildSpec = #{
        id => simulation_supervisor,
        start => {simulation_supervisor, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => supervisor,
        modules => [simulation_supervisor]
    },
    
    case supervisor:start_child(logistics_sim_sup, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Error ->
            io:format("Failed to start simulation supervisor: ~p~n", [Error]),
            Error
    end.

%% התחלת כל רכיבי הסימולציה
start_simulation_components(SupPid, Config) ->
    try
        %% פירוק ההגדרות
        Zones = maps:get(zones, Config, ["north", "center", "south"]),
        NumCouriers = maps:get(num_couriers, Config, 8),
        OrderInterval = maps:get(order_interval, Config, 5000),
        MinTravelTime = maps:get(min_travel_time, Config, 10000),
        MaxTravelTime = maps:get(max_travel_time, Config, 60000),
        
        %% עדכון הגדרות ב-ETS
        case ets:info(simulation_config) of
            undefined -> 
                ets:new(simulation_config, [named_table, public, {keypos, 1}]);
            _ -> 
                ok
        end,
        
        %% שמירת כל ההגדרות
        ets:insert(simulation_config, {travel_times, MinTravelTime, MaxTravelTime}),
        ets:insert(simulation_config, {num_couriers, NumCouriers}),
        ets:insert(simulation_config, {order_interval, OrderInterval}),
        ets:insert(simulation_config, {zones, Zones}),
        
        io:format("Saved travel times to ETS: Min=~p ms, Max=~p ms~n", [MinTravelTime, MaxTravelTime]),
        
        %% התחלת Courier Pool
        start_courier_pool(SupPid),
        
        %% התחלת Zone Managers
        lists:foreach(fun(Zone) ->
            start_zone_manager(SupPid, Zone)
        end, Zones),
        
        %% התחלת Couriers
        start_couriers(SupPid, NumCouriers),
        
        %% התחלת Random Order Generator עם interval מותאם
        start_order_generator(SupPid, OrderInterval),
        
        %% תן זמן לכל הרכיבים להתאתחל
        timer:sleep(1000),
        
        ok
    catch
        Type:Error ->
            io:format("Error starting simulation components: ~p:~p~n", [Type, Error]),
            {error, Error}
    end.

%% התחלת Courier Pool
start_courier_pool(SupPid) ->
    %% קבלת מספר השליחים מההגדרות
    NumCouriers = case ets:info(simulation_config) of
        undefined -> 8;  % ברירת מחדל
        _ ->
            case ets:lookup(simulation_config, num_couriers) of
                [{num_couriers, N}] -> N;
                [] -> 8  % ברירת מחדל
            end
    end,
    
    ChildSpec = #{
        id => sim_courier_pool,
        start => {courier_pool, start_link, [NumCouriers]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [courier_pool]
    },
    
    case supervisor:start_child(SupPid, ChildSpec) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> throw({courier_pool_start_failed, Error})
    end.

%% התחלת Zone Manager
start_zone_manager(SupPid, Zone) ->
    ChildSpec = #{
        id => list_to_atom("sim_zone_manager_" ++ Zone),
        start => {zone_manager, start_link, [Zone]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [zone_manager]
    },
    
    case supervisor:start_child(SupPid, ChildSpec) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> throw({zone_manager_start_failed, Zone, Error})
    end.

%% התחלת שליחים
start_couriers(SupPid, NumCouriers) ->
    lists:foreach(fun(N) ->
        CourierId = "courier" ++ integer_to_list(N),
        ChildSpec = #{
            id => list_to_atom("sim_" ++ CourierId),
            start => {courier, start_link, [CourierId]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [courier]
        },
        
        case supervisor:start_child(SupPid, ChildSpec) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            Error -> io:format("Warning: Failed to start courier ~p: ~p~n", [CourierId, Error])
        end
    end, lists:seq(1, NumCouriers)).

%% התחלת מחולל הזמנות
start_order_generator(SupPid, OrderInterval) ->
    ChildSpec = #{
        id => sim_random_order_generator,
        start => {random_order_generator, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [random_order_generator]
    },
    
    case supervisor:start_child(SupPid, ChildSpec) of
        {ok, _} -> 
            %% הגדרת interval אם נדרש
            random_order_generator:set_interval(OrderInterval),
            ok;
        {error, {already_started, _}} -> 
            random_order_generator:set_interval(OrderInterval),
            ok;
        Error -> 
            throw({order_generator_start_failed, Error})
    end.

%% עצירת כל רכיבי הסימולציה
stop_all_simulation_components(State) ->
    io:format("Stopping all simulation components...~n"),
    
    %% קבלת ה-supervisor pid
    case maps:get(simulation_sup, State, undefined) of
        undefined ->
            io:format("No simulation supervisor found~n"),
            ok;
        SupPid ->
            %% עצירת כל ה-children
            Children = supervisor:which_children(SupPid),
            lists:foreach(fun({Id, _, _, _}) ->
                supervisor:terminate_child(SupPid, Id),
                supervisor:delete_child(SupPid, Id)
            end, Children),
            
            %% עצירת הסופרווייזר עצמו
            supervisor:terminate_child(logistics_sim_sup, simulation_supervisor),
            supervisor:delete_child(logistics_sim_sup, simulation_supervisor)
    end,
    
    %% ניקוי ETS tables
    clear_ets_tables(),
    
    ok.

%% ניקוי טבלאות ETS
clear_ets_tables() ->
    %% ניקוי courier_states
    case ets:info(courier_states) of
        undefined -> ok;
        _ -> ets:delete_all_objects(courier_states)
    end,
    
    %% ניקוי package_states
    case ets:info(package_states) of
        undefined -> ok;
        _ -> ets:delete_all_objects(package_states)
    end,
    
    %% ניקוי zone_states
    case ets:info(zone_states) of
        undefined -> ok;
        _ -> ets:delete_all_objects(zone_states)
    end.

%% איפוס המצב
reset_state() ->
    #{
        simulation_config => #{},
        simulation_sup => undefined,
        zones => [],
        healthy_zones => [],
        failed_zones => [],
        start_time => undefined
    }.

%% דיווח על מצב הסימולציה ל-WebSocket handlers
report_simulation_state(State, Config) ->
    %% שליחת הודעה לכל ה-WebSocket handlers דרך State Collector
    case whereis(logistics_state_collector) of
        undefined -> 
            ok;
        _ ->
            %% נשתמש בפונקציה חדשה ב-State Collector
            logistics_state_collector:simulation_state_changed(State, Config)
    end.

%% בדיקה שכל האזורים מוכנים ופעילים
check_all_zones_ready(State) ->
    Zones = maps:get(zones, State),
    lists:all(fun(Zone) ->
        case whereis(list_to_atom("zone_manager_" ++ Zone)) of
            undefined -> false;
            _Pid -> true
        end
    end, Zones).

%% בדיקת בריאות האזורים
check_zones_health(State) ->
    Zones = maps:get(zones, State),
    lists:filter(fun(Zone) ->
        case whereis(list_to_atom("zone_manager_" ++ Zone)) of
            undefined -> true;
            Pid -> 
                case is_process_alive(Pid) of
                    false -> true;
                    true -> false
                end
        end
    end, Zones).

%% טיפול בכשל של אזור
handle_zone_failure(Zone) ->
    io:format("Handling failure of zone: ~p~n", [Zone]),
    ok.

%% השהיית כל מחוללי ההזמנות
pause_all_order_generators(_State) ->
    %% השהיית המחולל הרנדומלי
    case whereis(random_order_generator) of
        undefined -> ok;
        _ -> random_order_generator:pause()
    end.

%% המשך כל מחוללי ההזמנות
resume_all_order_generators(_State) ->
    %% המשך המחולל הרנדומלי
    case whereis(random_order_generator) of
        undefined -> ok;
        _ -> random_order_generator:resume()
    end.

%% התחלת תהליך כיבוי מסודר
initiate_graceful_shutdown(State) ->
    io:format("Initiating graceful shutdown of all zones~n"),
    stop_all_simulation_components(State),
    erlang:send_after(1000, self(), {shutdown_complete}).

%% בדיקה אם כל האזורים סיימו את תהליך הכיבוי
all_zones_shutdown(_State) ->
    true.

%% -----------------------------------------------------------
%% Callbacks נדרשים עבור gen_statem
%% -----------------------------------------------------------

terminate(_Reason, _State, _Data) -> 
    io:format("Control Center terminating~n"),
    ok.

code_change(_OldVsn, State, Data, _Extra) -> 
    {ok, State, Data}.