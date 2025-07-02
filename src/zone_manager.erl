%% -----------------------------------------------------------
%% מודול מנהל אזור (Zone Manager) - FSM
%% אחראי על חלוקת משלוחים ושליחים באזור מסוים
%% תיקון: שליחת ספירת חבילות נכונה לממשק
%% -----------------------------------------------------------

-module(zone_manager).
-behaviour(gen_statem).

%% API - הוספתי exports לכל הפונקציות הציבוריות
-export([start_link/1, new_package/2]).

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

    %% דיווח ראשוני למערכת הניטור, כולל המונה החדש
    report_zone_state(ZoneName, #{
        waiting_packages => [], active_deliveries => 0,
        total_deliveries => 0, failed_deliveries => 0, total_orders => 0
    }),

    {ok, monitoring, #{
        zone => ZoneName,
        waiting_packages => [],
        active_deliveries => 0,
        total_deliveries => 0,
        failed_deliveries => 0,
        %% הוספת מונה לסך כל ההזמנות
        total_orders => 0
    }}.

%% -----------------------------------------------------------
%% API - הכנסת חבילה חדשה (הוזמנה ע"י לקוח/תהליך חיצוני)
%% -----------------------------------------------------------
new_package(Zone, PackageId) ->
    io:format("API: Sending new_package ~p to zone ~p~n", [PackageId, Zone]),
    gen_statem:cast(list_to_atom("zone_manager_" ++ Zone), {new_package, PackageId}).

%% -----------------------------------------------------------
%% פונקציה להדפסת מצב DEBUG: שליחים זמינים, חבילות ממתינות
%% -----------------------------------------------------------
debug_state(Data) ->
    Waiting = maps:get(waiting_packages, Data, []),
    Active = maps:get(active_deliveries, Data, 0),
    Total = maps:get(total_deliveries, Data, 0),
    Failed = maps:get(failed_deliveries, Data, 0),
    TotalOrders = maps:get(total_orders, Data, 0),
    io:format(
        ">>> Zone ~p DEBUG: Total: ~p, Waiting: ~p, Active: ~p, Delivered: ~p, Failed: ~p~n",
        [maps:get(zone, Data), TotalOrders, length(Waiting), Active, Total, Failed]
    ).

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

%% טיפול בחבילה חדשה במצב monitoring
handle_event(cast, {new_package, PackageId}, monitoring, Data) ->
    debug_state(Data),
    Zone = maps:get(zone, Data),
    io:format("Zone(~p) received new package: ~p~n", [Zone, PackageId]),

    %% עדכון מונה סך ההזמנות ועדכון המצב
    TotalOrders = maps:get(total_orders, Data) + 1,
    DataWithTotal = Data#{total_orders => TotalOrders},

    case whereis(list_to_atom("package_" ++ PackageId)) of
        undefined ->
            {ok, _} = package:start_link(PackageId, Zone);
        _ ->
            ok
    end,

    case courier_pool:request_courier(Zone) of
        {ok, Courier} ->
            io:format("Zone(~p) got courier ~p from pool for package ~p~n", [Zone, Courier, PackageId]),
            package:assign_courier(PackageId, Courier),
            ActiveDeliveries = maps:get(active_deliveries, DataWithTotal, 0),
            NewData = DataWithTotal#{active_deliveries => ActiveDeliveries + 1},
            report_zone_state(Zone, NewData),
            debug_state(NewData),
            {keep_state, NewData};
        {error, no_couriers_available} ->
            io:format("Zone(~p): No couriers available, package ~p waits in queue~n", [Zone, PackageId]),
            Waiting = maps:get(waiting_packages, DataWithTotal),
            NewData = DataWithTotal#{waiting_packages => Waiting ++ [PackageId]},
            report_zone_state(Zone, NewData),
            debug_state(NewData),
            {keep_state, NewData}
    end;

%% טיפול בהקצאה ישירה מהמאגר המרכזי
handle_event(cast, {assign_to_waiting_package, CourierId}, monitoring, Data) ->
    debug_state(Data),
    Zone = maps:get(zone, Data),
    Waiting = maps:get(waiting_packages, Data),
    case Waiting of
        [Pkg | RestPkgs] ->
            io:format("Zone(~p): Got courier ~p from pool for waiting package ~p~n", [Zone, CourierId, Pkg]),
            package:assign_courier(Pkg, CourierId),
            ActiveDeliveries = maps:get(active_deliveries, Data, 0),
            NewData = Data#{
                waiting_packages => RestPkgs,
                active_deliveries => ActiveDeliveries + 1
            },
            report_zone_state(Zone, NewData),
            debug_state(NewData),
            {keep_state, NewData};
        [] ->
            io:format("Zone(~p): Got assignment ~p but have no waiting packages. Returning courier.~n", [Zone, CourierId]),
            courier_pool:return_courier(CourierId),
            {keep_state, Data}
    end;

%% טיפול בהודעת סיום משלוח מוצלח
handle_event(cast, {package_delivered, PackageId, CourierId}, monitoring, Data) ->
    debug_state(Data),
    Zone = maps:get(zone, Data),
    io:format("Zone(~p): Package ~p delivered by courier ~p!~n", [Zone, PackageId, CourierId]),
    Total = maps:get(total_deliveries, Data, 0),
    ActiveDeliveries = maps:get(active_deliveries, Data, 0),
    NewData = Data#{
        total_deliveries => Total + 1,
        active_deliveries => max(0, ActiveDeliveries - 1)
    },
    report_zone_state(Zone, NewData),
    {keep_state, NewData};

%% טיפול בכשל הקצאה - שליח תפוס
handle_event(cast, {assignment_failed, PackageId, CourierId}, monitoring, Data) ->
    debug_state(Data),
    Zone = maps:get(zone, Data),
    ExpectedPrefix = Zone ++ "_",
    case string:prefix(PackageId, ExpectedPrefix) of
        nomatch ->
            io:format("Zone(~p): Ignoring assignment failure for package ~p (belongs to different zone)~n", [Zone, PackageId]),
            {keep_state, Data};
        _ ->
            io:format("Zone(~p): Assignment failed - courier ~p busy, requeueing package ~p~n", [Zone, CourierId, PackageId]),
            io:format("Zone(~p): Returning unutilized courier ~p to the pool.~n", [Zone, CourierId]),
            courier_pool:return_courier(CourierId),
            Waiting = maps:get(waiting_packages, Data),
            Failed = maps:get(failed_deliveries, Data, 0),
            ActiveDeliveries = maps:get(active_deliveries, Data, 0),
            NewData = Data#{
                waiting_packages => Waiting ++ [PackageId],
                failed_deliveries => Failed + 1,
                active_deliveries => max(0, ActiveDeliveries - 1)
            },
            report_zone_state(Zone, NewData),
            debug_state(NewData),
            {keep_state, NewData}
    end;

%% התחלת מחזור אופטימיזציה
handle_event(cast, {start_optimization}, monitoring, Data) ->
    io:format("Zone(~p): Starting optimization cycle~n", [maps:get(zone, Data)]),
    {next_state, optimizing, Data};

%% זיהוי עומס יתר באזור
handle_event(cast, {overload_detected}, monitoring, Data) ->
    io:format("Zone(~p): Overload detected, entering emergency mode~n", [maps:get(zone, Data)]),
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
            package:assign_courier(Pkg, CourierId),
            NewData = Data#{waiting_packages => RestPkgs},
            case RestPkgs of
                [] -> {next_state, monitoring, NewData};
                _ -> {keep_state, NewData}
            end;
        [] ->
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
    NewData = Data#{waiting_packages => NewWaiting},
    {keep_state, NewData};

%% קבלת סטטיסטיקות האזור
handle_event({call, From}, get_stats, _StateName, Data) ->
    WaitingPackages = maps:get(waiting_packages, Data, []),
    Stats = #{
        zone => maps:get(zone, Data),
        waiting_packages => WaitingPackages,
        waiting_count => length(WaitingPackages),
        active_deliveries => maps:get(active_deliveries, Data, 0),
        total_deliveries => maps:get(total_deliveries, Data, 0),
        failed_deliveries => maps:get(failed_deliveries, Data, 0)
    },
    {keep_state, Data, [{reply, From, Stats}]};

%% טיפול בהודעה ששליח נתפס על ידי אזור אחר
handle_event(cast, {courier_taken, CourierId}, _StateName, Data) ->
    Avail = maps:get(available_couriers, Data),
    case lists:member(CourierId, Avail) of
        true ->
            NewAvail = lists:delete(CourierId, Avail),
            io:format("Zone(~p): Removing courier ~p from available list (taken by another zone)~n", [maps:get(zone, Data), CourierId]),
            NewData = Data#{available_couriers => NewAvail},
            debug_state(NewData),
            {keep_state, NewData};
        false ->
            {keep_state, Data}
    end;

%% catch-all לאירועים לא מזוהים
handle_event(EventType, Event, StateName, Data) ->
    debug_state(Data),
    io:format("Zone(~p) in state ~p received unhandled event: ~p (~p)~n", [maps:get(zone, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------
report_zone_state(Zone, Data) ->
    case whereis(logistics_state_collector) of
        undefined ->
            io:format("DEBUG: State Collector not available for zone ~p state update~n", [Zone]);
        _ ->
            %% >> התיקון הקריטי כאן <<
            %% בניית מפת נתונים חדשה עם ספירות נכונות, במקום שליחת המצב הפנימי
            StateData = #{
                waiting_packages => length(maps:get(waiting_packages, Data, [])),
                active_deliveries => maps:get(active_deliveries, Data, 0),
                total_delivered => maps:get(total_deliveries, Data, 0),
                failed_deliveries => maps:get(failed_deliveries, Data, 0),
                total_orders => maps:get(total_orders, Data, 0)
            },
            logistics_state_collector:zone_state_changed(Zone, StateData)
    end.

%% -----------------------------------------------------------
%% דרישות gen_statem
%% -----------------------------------------------------------
terminate(_Reason, _State, Data) ->
    io:format("Zone Manager ~p terminating~n", [maps:get(zone, Data)]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
