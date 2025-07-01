-module(control_center).
-behaviour(gen_statem).

%% API - הוספתי פונקציות API נוספות
-export([start_link/0, pause_simulation/0, resume_simulation/0, get_status/0, emergency_stop/0]).

%% Callbacks - הוספתי exports לכל הפונקציות הנדרשות
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([initializing/3, running/3, paused/3, degraded/3, recovering/3, shutting_down/3, halted/3]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() ->
    state_functions.

init([]) ->
    io:format("Control Center initializing...~n"),
    %% הוספתי אתחול טיימר לבדיקת תקינות אזורים
    erlang:send_after(5000, self(), check_zones_health),
    {ok, initializing, #{
        zones => ["north", "center", "south"],
        healthy_zones => [],
        failed_zones => [],
        start_time => erlang:system_time(second)
    }}.

%%%% States

%% מצב אתחול - בודק שכל האזורים פעילים
initializing({system_start, _}, _From, State) ->
    io:format("System start received, checking all zones...~n"),
    case check_all_zones_ready(State) of
        true ->
            io:format("All zones ready -> Transitioning to Running~n"),
            {next_state, running, State#{healthy_zones => maps:get(zones, State)}};
        false ->
            io:format("Not all zones ready, retrying in 2 seconds...~n"),
            erlang:send_after(2000, self(), {system_start, retry}),
            {keep_state, State}
    end;

initializing(check_zones_health, _From, State) ->
    io:format("Checking zones health during initialization...~n"),
    case check_all_zones_ready(State) of
        true ->
            io:format("All zones are healthy -> Starting simulation~n"),
            {next_state, running, State#{healthy_zones => maps:get(zones, State)}};
        false ->
            %% ממשיך לבדוק כל 5 שניות
            erlang:send_after(5000, self(), check_zones_health),
            {keep_state, State}
    end;

initializing(EventType, _From, State) ->
    io:format("Initializing received event: ~p~n", [EventType]),
    {keep_state, State}.

%% מצב ריצה רגיל - המערכת פועלת תקין
running({pause, _}, _From, State) ->
    io:format("Simulation paused by operator~n"),
    pause_all_order_generators(State),
    {next_state, paused, State};

running({zone_failure_detected, Zone}, _From, State) ->
    io:format("Zone failure detected: ~p -> Entering degraded mode~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    handle_zone_failure(Zone),
    {next_state, degraded, NewState};

running(check_zones_health, _From, State) ->
    %% בדיקה תקופתית של בריאות האזורים
    FailedZones = check_zones_health(State),
    case FailedZones of
        [] ->
            %% כל האזורים תקינים
            erlang:send_after(10000, self(), check_zones_health),
            {keep_state, State};
        Failed ->
            %% יש אזורים שנכשלו
            io:format("Zones failed health check: ~p~n", [Failed]),
            gen_statem:cast(self(), {zone_failure_detected, hd(Failed)}),
            {keep_state, State}
    end;

running({shutdown_requested}, _From, State) ->
    io:format("Shutdown requested -> Starting graceful shutdown~n"),
    initiate_graceful_shutdown(),
    {next_state, shutting_down, State};

running(Event, _From, State) ->
    io:format("Running received event: ~p~n", [Event]),
    {keep_state, State}.

%% מצב השהיה - הסימולציה מושהית
paused({resume, _}, _From, State) ->
    io:format("Simulation resumed by operator~n"),
    resume_all_order_generators(State),
    %% חזרה לבדיקת בריאות תקופתית
    erlang:send_after(10000, self(), check_zones_health),
    {next_state, running, State};

paused({zone_failure_detected, Zone}, _From, State) ->
    io:format("Zone failure detected while paused: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    {next_state, degraded, NewState};

paused(Event, _From, State) ->
    io:format("Paused received event: ~p~n", [Event]),
    {keep_state, State}.

%% מצב פגום - אזור אחד או יותר לא פועל
degraded({zone_recovered, Zone}, _From, State) ->
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
            {next_state, running, NewState};
        _ ->
            io:format("Still have failed zones: ~p~n", [maps:get(failed_zones, NewState)]),
            {keep_state, NewState}
    end;

degraded({attempt_recovery}, _From, State) ->
    io:format("Attempting to recover failed zones~n"),
    {next_state, recovering, State};

degraded({zone_failure_detected, Zone}, _From, State) ->
    io:format("Additional zone failure: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    handle_zone_failure(Zone),
    {keep_state, NewState};

degraded(Event, _From, State) ->
    io:format("Degraded received event: ~p~n", [Event]),
    {keep_state, State}.

%% מצב התאוששות - מנסה להחזיר אזורים שנכשלו
recovering({zone_recovered, Zone}, _From, State) ->
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
            {next_state, running, NewState};
        _ ->
            {keep_state, NewState}
    end;

recovering({recovery_timeout}, _From, State) ->
    io:format("Recovery timeout -> Returning to degraded mode~n"),
    {next_state, degraded, State};

recovering(Event, _From, State) ->
    io:format("Recovering received event: ~p~n", [Event]),
    {keep_state, State}.

%% מצב כיבוי - סוגר את המערכת בצורה מסודרת
shutting_down({shutdown_complete}, _From, State) ->
    io:format("All systems shutdown complete~n"),
    {next_state, halted, State};

shutting_down({zone_shutdown_complete, Zone}, _From, State) ->
    io:format("Zone ~p shutdown complete~n", [Zone]),
    %% בודק אם כל האזורים סגרו
    case all_zones_shutdown(State) of
        true ->
            gen_statem:cast(self(), {shutdown_complete}),
            {keep_state, State};
        false ->
            {keep_state, State}
    end;

shutting_down(Event, _From, State) ->
    io:format("Shutting down received event: ~p~n", [Event]),
    {keep_state, State}.

%% מצב סופי - המערכת סגורה
halted(Event, _From, State) ->
    io:format("System halted, ignoring event: ~p~n", [Event]),
    {keep_state, State}.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

%% בדיקה שכל האזורים מוכנים
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
            undefined -> true;  % אזור לא פעיל
            Pid -> 
                case is_process_alive(Pid) of
                    false -> true;  % תהליך מת
                    true -> false   % תהליך בריא
                end
        end
    end, Zones).

%% תיקון warning: הסרתי את הפרמטר State שלא היה בשימוש
handle_zone_failure(Zone) ->
    io:format("Handling failure of zone: ~p~n", [Zone]),
    %% כאן אפשר להוסיף לוגיקה להעברת חבילות לאזורים אחרים
    %% התאוששות תהליכים וכו'
    ok.

%% השהיית כל מחוללי ההזמנות
pause_all_order_generators(State) ->
    Zones = maps:get(zones, State),
    lists:foreach(fun(Zone) ->
        case whereis(list_to_atom("order_gen_" ++ Zone)) of
            undefined -> ok;
            _Pid -> order_generator:pause(Zone)
        end
    end, Zones).

%% המשך כל מחוללי ההזמנות
resume_all_order_generators(State) ->
    Zones = maps:get(zones, State),
    lists:foreach(fun(Zone) ->
        case whereis(list_to_atom("order_gen_" ++ Zone)) of
            undefined -> ok;
            _Pid -> order_generator:resume(Zone)
        end
    end, Zones).

%% תיקון warning: הסרתי את הפרמטר State שלא היה בשימוש
initiate_graceful_shutdown() ->
    io:format("Initiating graceful shutdown of all zones~n"),
    %% כאן נוכל להוסיף לוגיקה לסגירה מסודרת
    erlang:send_after(5000, self(), {shutdown_complete}).

%% בדיקה אם כל האזורים נסגרו
all_zones_shutdown(_State) ->
    %% כרגע מחזיר true, אפשר להוסיף לוגיקה מורכבת יותר
    true.

%% -----------------------------------------------------------
%% API Functions - פונקציות ציבוריות לשליטה במרכז הבקרה
%% -----------------------------------------------------------

pause_simulation() ->
    gen_statem:cast(?MODULE, {pause, operator}).

resume_simulation() ->
    gen_statem:cast(?MODULE, {resume, operator}).

get_status() ->
    gen_statem:call(?MODULE, get_status).

emergency_stop() ->
    gen_statem:cast(?MODULE, {shutdown_requested}).

%% -----------------------------------------------------------
%% Callbacks נדרשים
%% -----------------------------------------------------------
terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.