%% -----------------------------------------------------------
%% מודול מנהל אזור (Zone Manager) - FSM
%% אחראי על חלוקת משלוחים ושליחים באזור מסוים
%% תיקון: עדכון רשימת השליחים לפי האזור
%% -----------------------------------------------------------

-module(zone_manager).
-behaviour(gen_statem).

%% API - הוספתי exports לכל הפונקציות הציבוריות
-export([start_link/1, new_package/2, courier_available/2]).

%% Callbacks - שינוי ל-handle_event mode
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% -----------------------------------------------------------
%% יצירת Zone Manager עבור אזור בשם zone_name (string)
%% -----------------------------------------------------------
start_link(ZoneName) ->
    gen_statem:start_link({local, list_to_atom("zone_manager_" ++ ZoneName)}, ?MODULE, [ZoneName], []).

%% תיקון: שינוי ל-handle_event mode
callback_mode() -> handle_event_function.

%% -----------------------------------------------------------
%% שמירת סטייט: רשימת שליחים זמינים, חבילות שממתינות, וכו'
%% כל השליחים זמינים לכל האזורים!
%% -----------------------------------------------------------
init([ZoneName]) ->
    io:format("Zone Manager ~p initializing...~n", [ZoneName]),
    
    io:format("Zone ~p initialized (using central courier pool)~n", [ZoneName]),
    
    {ok, monitoring, #{
        zone => ZoneName,
        waiting_packages => [],
        total_deliveries => 0,  %% מונה משלוחים כולל
        failed_deliveries => 0  %% מונה משלוחים שנכשלו
    }}.

%% -----------------------------------------------------------
%% API - הכנסת חבילה חדשה (הוזמנה ע"י לקוח/תהליך חיצוני)
%% -----------------------------------------------------------
new_package(Zone, PackageId) ->
    io:format("API: Sending new_package ~p to zone ~p~n", [PackageId, Zone]),
    gen_statem:cast(list_to_atom("zone_manager_" ++ Zone), {new_package, PackageId}).

%% API - שליח מתפנה
courier_available(Zone, CourierId) ->
    io:format("API: Courier ~p available in zone ~p~n", [CourierId, Zone]),
    gen_statem:cast(list_to_atom("zone_manager_" ++ Zone), {courier_available, CourierId}).

%% -----------------------------------------------------------
%% פונקציה להדפסת מצב DEBUG: שליחים זמינים, חבילות ממתינות
%% -----------------------------------------------------------
debug_state(Data) ->
    Waiting = maps:get(waiting_packages, Data, []),
    Total = maps:get(total_deliveries, Data, 0),
    Failed = maps:get(failed_deliveries, Data, 0),
    io:format(
        ">>> Zone ~p DEBUG: Waiting packages: ~p (~p total), Total deliveries: ~p, Failed: ~p~n",
        [maps:get(zone, Data), Waiting, length(Waiting), Total, Failed]
    ).

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

%% טיפול בחבילה חדשה במצב monitoring
handle_event(cast, {new_package, PackageId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p) received new package: ~p~n", [maps:get(zone, Data), PackageId]),
    
    %% בקש שליח מהתור המרכזי
    case courier_pool:request_courier() of
        {ok, Courier} ->
            %% קיבלנו שליח - מקצה אותו לחבילה
            io:format("Zone(~p) got courier ~p from pool for package ~p~n", 
                     [maps:get(zone, Data), Courier, PackageId]),
            %% יצירת תהליך חבילה
            {ok, _Pid} = package:start_link(PackageId, list_to_atom("zone_manager_" ++ maps:get(zone, Data))),
            %% הקצאת השליח לחבילה
            package:assign_courier(PackageId, Courier),
            debug_state(Data),
            {keep_state, Data};
        {error, no_couriers_available} ->
            %% אין שליחים פנויים - החבילה נכנסת לתור המתנה
            io:format("Zone(~p): No couriers available, package ~p waits in queue~n", 
                     [maps:get(zone, Data), PackageId]),
            Waiting = maps:get(waiting_packages, Data),
            NewData = Data#{waiting_packages => Waiting ++ [PackageId]},
            debug_state(NewData),
            {keep_state, NewData}
    end;

%% טיפול בשליח שהתפנה - בדוק אם יש חבילות ממתינות באזור
handle_event(cast, {courier_available, CourierId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p): Notified that courier ~p might be available~n", [maps:get(zone, Data), CourierId]),
    
    Waiting = maps:get(waiting_packages, Data),
    case Waiting of
        [Pkg | RestPkgs] ->
            %% יש חבילה ממתינה - נסה לקבל שליח מהתור
            case courier_pool:request_courier() of
                {ok, AssignedCourier} ->
                    %% קיבלנו שליח (אולי אותו אחד, אולי אחר)
                    io:format("Zone(~p): Got courier ~p from pool for waiting package ~p~n", 
                             [maps:get(zone, Data), AssignedCourier, Pkg]),
                    %% וידוא שתהליך החבילה קיים
                    case whereis(list_to_atom("package_" ++ Pkg)) of
                        undefined ->
                            {ok, _Pid} = package:start_link(Pkg, list_to_atom("zone_manager_" ++ maps:get(zone, Data)));
                        _Pid ->
                            ok
                    end,
                    package:assign_courier(Pkg, AssignedCourier),
                    NewData = Data#{waiting_packages => RestPkgs},
                    debug_state(NewData),
                    {keep_state, NewData};
                {error, no_couriers_available} ->
                    %% אין שליחים פנויים - השאר את החבילה בתור
                    io:format("Zone(~p): No couriers available for waiting package~n", [maps:get(zone, Data)]),
                    {keep_state, Data}
            end;
        [] ->
            %% אין חבילות ממתינות באזור הזה
            io:format("Zone(~p): No waiting packages~n", [maps:get(zone, Data)]),
            {keep_state, Data}
    end;

%% טיפול בהודעת סיום משלוח מוצלח
handle_event(cast, {package_delivered, PackageId, CourierId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p): Package ~p delivered by courier ~p!~n", [maps:get(zone, Data), PackageId, CourierId]),
    %% עדכון סטטיסטיקות
    Total = maps:get(total_deliveries, Data, 0),
    NewData = Data#{total_deliveries => Total + 1},
    {keep_state, NewData};

%% טיפול בכשל הקצאה - שליח תפוס
handle_event(cast, {assignment_failed, PackageId, CourierId}, monitoring, Data) ->
    debug_state(Data),
    %% בדיקה שהחבילה שייכת לאזור הזה
    Zone = maps:get(zone, Data),
    ExpectedPrefix = Zone ++ "_",
    case string:prefix(PackageId, ExpectedPrefix) of
        nomatch ->
            %% החבילה לא שייכת לאזור הזה - מתעלם
            io:format("Zone(~p): Ignoring assignment failure for package ~p (belongs to different zone)~n", 
                      [Zone, PackageId]),
            {keep_state, Data};
        _ ->
            %% החבילה שייכת לאזור הזה
            io:format("Zone(~p): Assignment failed - courier ~p busy, requeueing package ~p~n", 
                      [Zone, CourierId, PackageId]),
            %% החזר את החבילה לתור ההמתנה
            Waiting = maps:get(waiting_packages, Data),
            %% עדכון סטטיסטיקות כשלונות
            Failed = maps:get(failed_deliveries, Data, 0),
            NewData = Data#{
                waiting_packages => Waiting ++ [PackageId],
                failed_deliveries => Failed + 1
            },
            debug_state(NewData),
            {keep_state, NewData}
    end;

%% התחלת מחזור אופטימיזציה
handle_event(cast, {start_optimization}, monitoring, Data) ->
    io:format("Zone(~p): Starting optimization cycle~n", [maps:get(zone, Data)]),
    %% TODO: ממש אלגוריתם אופטימיזציה למסלולים
    {next_state, optimizing, Data};

%% זיהוי עומס יתר באזור
handle_event(cast, {overload_detected}, monitoring, Data) ->
    io:format("Zone(~p): Overload detected, entering emergency mode~n", [maps:get(zone, Data)]),
    %% TODO: בקש עזרה מאזורים סמוכים
    {next_state, emergency_mode, Data};

%% מצב אופטימיזציה - סיום האופטימיזציה
handle_event(cast, {optimization_complete}, optimizing, Data) ->
    io:format("Zone(~p): Optimization complete, returning to monitoring~n", [maps:get(zone, Data)]),
    {next_state, monitoring, Data};

%% מצב אופטימיזציה - חבילה חדשה מתקבלת
handle_event(cast, {new_package, PackageId}, optimizing, Data) ->
    io:format("Zone(~p): Package ~p queued during optimization~n", [maps:get(zone, Data), PackageId]),
    Waiting = maps:get(waiting_packages, Data),
    NewData = Data#{waiting_packages => Waiting ++ [PackageId]},
    {keep_state, NewData};

%% מצב חירום - איזון עומסים הושלם
handle_event(cast, {load_balanced}, emergency_mode, Data) ->
    io:format("Zone(~p): Load balanced, returning to normal operation~n", [maps:get(zone, Data)]),
    {next_state, monitoring, Data};

%% מצב חירום - שליח מתפנה
handle_event(cast, {courier_available, CourierId}, emergency_mode, Data) ->
    io:format("Zone(~p): Emergency - courier ~p available~n", [maps:get(zone, Data), CourierId]),
    Waiting = maps:get(waiting_packages, Data),
    case Waiting of
        [Pkg | RestPkgs] ->
            %% הקצאה דחופה של החבילה הראשונה בתור
            package:assign_courier(Pkg, CourierId),
            NewData = Data#{waiting_packages => RestPkgs},
            case RestPkgs of
                [] -> {next_state, monitoring, NewData};  %% אין יותר חבילות - חזרה למצב רגיל
                _ -> {keep_state, NewData}  %% עדיין יש חבילות - נשאר במצב חירום
            end;
        [] ->
            %% אין חבילות ממתינות - חזרה למצב רגיל
            Avail = maps:get(available_couriers, Data),
            NewData = Data#{available_couriers => Avail ++ [CourierId]},
            {next_state, monitoring, NewData}
    end;

%% מצב איזון עומסים - סיום איזון
handle_event(cast, {load_balance_complete}, load_balancing, Data) ->
    io:format("Zone(~p): Load balancing complete~n", [maps:get(zone, Data)]),
    {next_state, monitoring, Data};

%% מצב איזון עומסים - העברת חבילה לאזור אחר
handle_event(cast, {transfer_package, PackageId, ToZone}, load_balancing, Data) ->
    io:format("Zone(~p): Transferring package ~p to zone ~p~n", [maps:get(zone, Data), PackageId, ToZone]),
    Waiting = maps:get(waiting_packages, Data),
    NewWaiting = lists:delete(PackageId, Waiting),
    %% TODO: יצירת חבילה חדשה באזור היעד
    NewData = Data#{waiting_packages => NewWaiting},
    {keep_state, NewData};

%% קבלת סטטיסטיקות האזור
handle_event({call, From}, get_stats, _StateName, Data) ->
    Stats = #{
        zone => maps:get(zone, Data),
        available_couriers => length(maps:get(available_couriers, Data)),
        waiting_packages => length(maps:get(waiting_packages, Data)),
        total_deliveries => maps:get(total_deliveries, Data, 0),
        failed_deliveries => maps:get(failed_deliveries, Data, 0)
    },
    {keep_state, Data, [{reply, From, Stats}]};

%% טיפול בהודעה ששליח נתפס על ידי אזור אחר
handle_event(cast, {courier_taken, CourierId}, _StateName, Data) ->
    %% הסר את השליח מרשימת הפנויים
    Avail = maps:get(available_couriers, Data),
    case lists:member(CourierId, Avail) of
        true ->
            NewAvail = lists:delete(CourierId, Avail),
            io:format("Zone(~p): Removing courier ~p from available list (taken by another zone)~n", 
                     [maps:get(zone, Data), CourierId]),
            NewData = Data#{available_couriers => NewAvail},
            debug_state(NewData),
            {keep_state, NewData};
        false ->
            %% השליח כבר לא ברשימה - אין מה לעשות
            {keep_state, Data}
    end;

%% catch-all לאירועים לא מזוהים
handle_event(EventType, Event, StateName, Data) ->
    debug_state(Data),
    io:format("Zone(~p) in state ~p received unhandled event: ~p (~p)~n", 
              [maps:get(zone, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%% -----------------------------------------------------------
%% דרישות gen_statem
%% -----------------------------------------------------------

%% פונקציה הנקראת כשהתהליך נסגר
terminate(_Reason, _State, Data) -> 
    io:format("Zone Manager ~p terminating~n", [maps:get(zone, Data)]),
    ok.

%% פונקציה לטיפול בשינוי גרסת קוד בזמן ריצה
code_change(_OldVsn, State, Data, _Extra) -> 
    {ok, State, Data}.