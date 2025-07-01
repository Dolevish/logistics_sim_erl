-module(control_center).
-behaviour(gen_statem).

%% API - הוספתי פונקציות API נוספות
-export([start_link/0, pause_simulation/0, resume_simulation/0, get_status/0, emergency_stop/0]).

%% Callbacks - הוספתי exports לכל הפונקציות הנדרשות
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([initializing/3, running/3, paused/3, degraded/3, recovering/3, shutting_down/3, halted/3]).

%% -----------------------------------------------------------
%% API Functions - פונקציות ציבוריות לשליטה במרכז הבקרה
%% -----------------------------------------------------------

%% התחלת תהליך מרכז הבקרה
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% קביעת מצב הפעולה של ה-FSM - state_functions
callback_mode() ->
    state_functions.

%% אתחול מרכז הבקרה
init([]) ->
    io:format("Control Center initializing...~n"),
    %% תיקון: שימוש נכון ב-send_after עם tuple
    erlang:send_after(5000, self(), {check_zones_health}),
    {ok, initializing, #{
        zones => ["north", "center", "south"],
        healthy_zones => [],
        failed_zones => [],
        start_time => erlang:system_time(second)
    }}.

%%%% States - מצבי ה-FSM השונים

%% מצב אתחול - בודק שכל האזורים פעילים
%% תיקון: gen_statem state functions מקבלות 3 פרמטרים, לא 4
initializing(cast, {system_start}, State) ->
    io:format("System start received, checking all zones...~n"),
    case check_all_zones_ready(State) of
        true ->
            io:format("All zones ready -> Transitioning to Running~n"),
            %% התחלת בדיקת תקינות תקופתית
            erlang:send_after(10000, self(), {check_zones_health}),
            {next_state, running, State#{healthy_zones => maps:get(zones, State)}};
        false ->
            io:format("Not all zones ready, retrying in 2 seconds...~n"),
            erlang:send_after(2000, self(), {retry_system_start}),
            {keep_state, State}
    end;

%% טיפול בהודעת בדיקת תקינות במצב אתחול
initializing(info, {check_zones_health}, State) ->
    io:format("Checking zones health during initialization...~n"),
    case check_all_zones_ready(State) of
        true ->
            io:format("All zones are healthy -> Starting simulation~n"),
            %% התחלת בדיקת תקינות תקופתית
            erlang:send_after(10000, self(), {check_zones_health}),
            {next_state, running, State#{healthy_zones => maps:get(zones, State)}};
        false ->
            %% ממשיך לבדוק כל 5 שניות
            erlang:send_after(5000, self(), {check_zones_health}),
            {keep_state, State}
    end;

%% טיפול בניסיון חוזר של התחלת המערכת
initializing(info, {retry_system_start}, State) ->
    %% שולח שוב את הודעת ההתחלה
    gen_statem:cast(self(), {system_start}),
    {keep_state, State};

%% כל אירוע אחר במצב אתחול
initializing(EventType, EventContent, State) ->
    io:format("Initializing received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב ריצה רגיל - המערכת פועלת תקין
running(cast, {pause}, State) ->
    io:format("Simulation paused by operator~n"),
    pause_all_order_generators(State),
    {next_state, paused, State};

%% זיהוי כשל באזור
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

%% בדיקה תקופתית של בריאות האזורים
running(info, {check_zones_health}, State) ->
    %% בדיקה תקופתית של בריאות האזורים
    FailedZones = check_zones_health(State),
    case FailedZones of
        [] ->
            %% כל האזורים תקינים - תזמון הבדיקה הבאה
            erlang:send_after(10000, self(), {check_zones_health}),
            {keep_state, State};
        Failed ->
            %% יש אזורים שנכשלו
            io:format("Zones failed health check: ~p~n", [Failed]),
            gen_statem:cast(self(), {zone_failure_detected, hd(Failed)}),
            {keep_state, State}
    end;

%% בקשת כיבוי מסודר
running(cast, {shutdown_requested}, State) ->
    io:format("Shutdown requested -> Starting graceful shutdown~n"),
    initiate_graceful_shutdown(),
    {next_state, shutting_down, State};

%% כל אירוע אחר במצב ריצה
running(EventType, EventContent, State) ->
    io:format("Running received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב השהיה - הסימולציה מושהית
paused(cast, {resume}, State) ->
    io:format("Simulation resumed by operator~n"),
    resume_all_order_generators(State),
    %% חזרה לבדיקת בריאות תקופתית
    erlang:send_after(10000, self(), {check_zones_health}),
    {next_state, running, State};

%% זיהוי כשל באזור גם במצב השהיה
paused(cast, {zone_failure_detected, Zone}, State) ->
    io:format("Zone failure detected while paused: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    {next_state, degraded, NewState};

%% כל אירוע אחר במצב השהיה
paused(EventType, EventContent, State) ->
    io:format("Paused received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב פגום - אזור אחד או יותר לא פועל
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
            %% חזרה לבדיקת תקינות תקופתית
            erlang:send_after(10000, self(), {check_zones_health}),
            {next_state, running, NewState};
        _ ->
            io:format("Still have failed zones: ~p~n", [maps:get(failed_zones, NewState)]),
            {keep_state, NewState}
    end;

%% ניסיון להחזיר אזורים שנכשלו
degraded(cast, {attempt_recovery}, State) ->
    io:format("Attempting to recover failed zones~n"),
    {next_state, recovering, State};

%% כשל נוסף באזור אחר
degraded(cast, {zone_failure_detected, Zone}, State) ->
    io:format("Additional zone failure: ~p~n", [Zone]),
    HealthyZones = maps:get(healthy_zones, State),
    FailedZones = maps:get(failed_zones, State),
    NewState = State#{
        healthy_zones => lists:delete(Zone, HealthyZones),
        failed_zones => [Zone | FailedZones]
    },
    handle_zone_failure(Zone),
    {keep_state, NewState};

%% כל אירוע אחר במצב פגום
degraded(EventType, EventContent, State) ->
    io:format("Degraded received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב התאוששות - מנסה להחזיר אזורים שנכשלו
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
            %% חזרה לבדיקת תקינות תקופתית
            erlang:send_after(10000, self(), {check_zones_health}),
            {next_state, running, NewState};
        _ ->
            {keep_state, NewState}
    end;

%% תם הזמן הקצוב להתאוששות
recovering(info, {recovery_timeout}, State) ->
    io:format("Recovery timeout -> Returning to degraded mode~n"),
    {next_state, degraded, State};

%% כל אירוע אחר במצב התאוששות
recovering(EventType, EventContent, State) ->
    io:format("Recovering received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב כיבוי - סוגר את המערכת בצורה מסודרת
shutting_down(info, {shutdown_complete}, State) ->
    io:format("All systems shutdown complete~n"),
    {next_state, halted, State};

%% אזור סיים את הכיבוי שלו
shutting_down(cast, {zone_shutdown_complete, Zone}, State) ->
    io:format("Zone ~p shutdown complete~n", [Zone]),
    %% בודק אם כל האזורים סגרו
    case all_zones_shutdown(State) of
        true ->
            erlang:send_after(100, self(), {shutdown_complete}),
            {keep_state, State};
        false ->
            {keep_state, State}
    end;

%% כל אירוע אחר במצב כיבוי
shutting_down(EventType, EventContent, State) ->
    io:format("Shutting down received event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% מצב סופי - המערכת סגורה
halted(EventType, EventContent, State) ->
    io:format("System halted, ignoring event: ~p (~p)~n", [EventContent, EventType]),
    {keep_state, State}.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

%% בדיקה שכל האזורים מוכנים ופעילים
check_all_zones_ready(State) ->
    Zones = maps:get(zones, State),
    lists:all(fun(Zone) -> 
        %% בודק אם תהליך מנהל האזור קיים
        case whereis(list_to_atom("zone_manager_" ++ Zone)) of
            undefined -> false;
            _Pid -> true
        end
    end, Zones).

%% בדיקת בריאות האזורים - מחזיר רשימת אזורים שנכשלו
check_zones_health(State) ->
    Zones = maps:get(zones, State),
    lists:filter(fun(Zone) ->
        %% בודק אם תהליך מנהל האזור חי ופעיל
        case whereis(list_to_atom("zone_manager_" ++ Zone)) of
            undefined -> true;  % אזור לא פעיל
            Pid -> 
                case is_process_alive(Pid) of
                    false -> true;  % תהליך מת
                    true -> false   % תהליך בריא
                end
        end
    end, Zones).

%% טיפול בכשל של אזור - העברת חבילות ושליחים לאזורים אחרים
handle_zone_failure(Zone) ->
    io:format("Handling failure of zone: ~p~n", [Zone]),
    %% TODO: כאן אפשר להוסיף לוגיקה להעברת חבילות לאזורים אחרים
    %% TODO: התאוששות תהליכים והעברת שליחים לאזורים פעילים
    ok.

%% השהיית כל מחוללי ההזמנות בכל האזורים
pause_all_order_generators(State) ->
    Zones = maps:get(zones, State),
    lists:foreach(fun(Zone) ->
        %% בודק אם מחולל ההזמנות של האזור פעיל
        case whereis(list_to_atom("order_gen_" ++ Zone)) of
            undefined -> ok;
            _Pid -> order_generator:pause(Zone)
        end
    end, Zones).

%% המשך כל מחוללי ההזמנות בכל האזורים
resume_all_order_generators(State) ->
    Zones = maps:get(zones, State),
    lists:foreach(fun(Zone) ->
        %% בודק אם מחולל ההזמנות של האזור פעיל
        case whereis(list_to_atom("order_gen_" ++ Zone)) of
            undefined -> ok;
            _Pid -> order_generator:resume(Zone)
        end
    end, Zones).

%% התחלת תהליך כיבוי מסודר של כל האזורים
initiate_graceful_shutdown() ->
    io:format("Initiating graceful shutdown of all zones~n"),
    %% TODO: כאן נוכל להוסיף לוגיקה לסגירה מסודרת של כל האזורים
    %% TODO: שליחת הודעות כיבוי לכל האזורים
    erlang:send_after(5000, self(), {shutdown_complete}).

%% בדיקה אם כל האזורים סיימו את תהליך הכיבוי
all_zones_shutdown(_State) ->
    %% TODO: כרגע מחזיר true, אפשר להוסיף לוגיקה מורכבת יותר
    %% TODO: בדיקה שכל האזורים באמת סגרו את כל התהליכים שלהם
    true.

%% -----------------------------------------------------------
%% API Functions - פונקציות ציבוריות לשליטה במרכז הבקרה
%% -----------------------------------------------------------

%% השהיית הסימולציה
pause_simulation() ->
    gen_statem:cast(?MODULE, {pause}).

%% המשך הסימולציה
resume_simulation() ->
    gen_statem:cast(?MODULE, {resume}).

%% קבלת סטטוס מרכז הבקרה
get_status() ->
    %% TODO: ממש call handler למצב הנוכחי
    {error, not_implemented}.

%% עצירת חירום של המערכת
emergency_stop() ->
    gen_statem:cast(?MODULE, {shutdown_requested}).

%% -----------------------------------------------------------
%% Callbacks נדרשים עבור gen_statem
%% -----------------------------------------------------------

%% פונקציה הנקראת כשהתהליך נסגר
terminate(_Reason, _State, _Data) -> 
    io:format("Control Center terminating~n"),
    ok.

%% פונקציה לטיפול בשינוי גרסת קוד בזמן ריצה
code_change(_OldVsn, State, Data, _Extra) -> 
    {ok, State, Data}.