%% -----------------------------------------------------------
%% מודול שליח (Courier) משופר - עם זמני נסיעה דינמיים
%% כל תהליך שליח מייצג שליח אמיתי במערכת
%% עם התקדמות אוטומטית בין המצבים וזמני נסיעה מותאמים אישית
%% -----------------------------------------------------------

-module(courier).
-behaviour(gen_statem).

%% ממשק API
-export([start_link/1]).

%% Callbacks - שינוי ל-handle_event mode
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% הגדרת האזורים הקבועים
-define(FIXED_ZONES, ["north", "center", "south"]).

%% -----------------------------------------------------------
%% יצירת שליח חדש (כולל שם דינמי לפי מזהה)
%% -----------------------------------------------------------
start_link(CourierId) ->
    gen_statem:start_link({local, list_to_atom("courier_" ++ CourierId)}, ?MODULE, [CourierId], []).

%% שינוי ל-handle_event mode
callback_mode() -> handle_event_function.

%% -----------------------------------------------------------
%% פונקציית init – אתחול התהליך במצב idle
%% -----------------------------------------------------------
init([CourierId]) ->
    io:format("Courier ~p starting idle, waiting for delivery assignments...~n", [CourierId]),
    %% אתחול מחולל המספרים הרנדומליים
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    
    %% דיווח למערכת הניטור על אתחול השליח - עם דיליי קטן
    erlang:send_after(100, self(), {report_initial_state}),

    {ok, idle, #{
        id => CourierId, 
        zones => ?FIXED_ZONES,  %% שימוש באזורים הקבועים
        delivered_packages => [], 
        total_delivered => 0
    }}.

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

%% מצב idle – שליח ממתין לקבלת משלוח חדש
handle_event(cast, {assign_delivery, PackageId}, idle, Data) ->
    CourierId = maps:get(id, Data),
    io:format("Courier(~p) received new assignment: package ~p - heading to restaurant!~n", [CourierId, PackageId]),
    %% עדכון סטטוס חבילה
    package:update_status(PackageId, picking_up),
    %% זמן נסיעה למסעדה רנדומלי לפי ההגדרות
    PickupMs = get_dynamic_travel_time(),
    io:format("Courier(~p) will arrive at restaurant for package ~p in ~p ms~n", [CourierId, PackageId, PickupMs]),

    %% דיווח למערכת הניטור על שינוי המצב
    report_state_change(CourierId, picking_up, #{package => PackageId, eta => PickupMs}),

    erlang:send_after(PickupMs, self(), pickup_complete),
    {next_state, picking_up, Data#{package => PackageId}};

%% תיקון: מטפל גם במקרה שיש אזור מקור
handle_event(cast, {assign_delivery, PackageId, FromZone}, idle, Data) ->
    %% דיווח על האזור שממנו הגיעה ההקצאה
    CourierId = maps:get(id, Data),
    io:format("Courier(~p) assigned package ~p from zone ~p~n", [CourierId, PackageId, FromZone]),

    %% עדכון נתוני השליח עם האזור
    NewData = Data#{zone => FromZone},

    %% קורא לטיפול הרגיל (בלי האזור)
    handle_event(cast, {assign_delivery, PackageId}, idle, NewData);

%% תיקון: שליח תפוס לא יכול לקבל הקצאות נוספות
handle_event(cast, {assign_delivery, PackageId, FromZone}, StateName, Data) when StateName =/= idle ->
    io:format("Courier(~p) BUSY in state ~p, rejecting package ~p from zone ~p~n",
              [maps:get(id, Data), StateName, PackageId, FromZone]),
    %% שולח הודעת כשל רק לאזור שביקש את ההקצאה
    ZoneManager = list_to_atom("zone_manager_" ++ FromZone),
    case whereis(ZoneManager) of
        undefined -> ok;
        _ -> gen_statem:cast(ZoneManager, {assignment_failed, PackageId, maps:get(id, Data)})
    end,
    {keep_state, Data};

%% מצב picking_up – שליח נוסע למסעדה לאיסוף
handle_event(info, pickup_complete, picking_up, Data) ->
    CourierId = maps:get(id, Data),
    PackageId = maps:get(package, Data),
    io:format("Courier(~p) arrived at restaurant, picking up package ~p!~n", [CourierId, PackageId]),
    package:update_status(PackageId, in_transit),
    %% זמן נסיעה ללקוח רנדומלי לפי ההגדרות
    DeliveryMs = get_dynamic_travel_time(),
    io:format("Courier(~p) heading to customer with package ~p, ETA: ~p ms~n", [CourierId, PackageId, DeliveryMs]),

    %% דיווח למערכת הניטור
    Zone = maps:get(zone, Data, "unknown"),
    report_state_change(CourierId, delivering, #{package => PackageId, zone => Zone, eta => DeliveryMs}),

    erlang:send_after(DeliveryMs, self(), delivery_complete),
    {next_state, delivering, Data};

%% מצב delivering – שליח בדרכו ללקוח
handle_event(info, delivery_complete, delivering, Data) ->
    CourierId = maps:get(id, Data),
    PackageId = maps:get(package, Data),
    io:format("Courier(~p) delivered package ~p, now available for next delivery!~n", [CourierId, PackageId]),
    package:update_status(PackageId, delivered),

    %% עדכון רשימת החבילות שנמסרו
    DeliveredPackages = maps:get(delivered_packages, Data),
    TotalDelivered = maps:get(total_delivered, Data),
    NewDeliveredPackages = [PackageId | DeliveredPackages],
    NewTotalDelivered = TotalDelivered + 1,

    %% דיווח למערכת הניטור על סיום המשלוח
    report_state_change(CourierId, idle, #{
        delivered_packages => NewDeliveredPackages,
        total_delivered => NewTotalDelivered
    }),

    %% החזר את השליח לתור המרכזי
    courier_pool:return_courier(CourierId),

    %% עדכון הנתונים המקומיים
    NewData = maps:remove(package, Data#{
        delivered_packages => NewDeliveredPackages,
        total_delivered => NewTotalDelivered,
        zone => null
    }),

    {next_state, idle, NewData};

%% מצב moving_zone – שמור לעתיד, מעבר אזורים
handle_event(EventType, Event, moving_zone, Data) ->
    io:format("Courier(~p) moving_zone: ~p (~p)~n", [maps:get(id, Data), Event, EventType]),
    {keep_state, Data};

%% catch-all לאירועים לא מזוהים - כולל הודעות debug
handle_event(EventType, Event, idle, Data) when EventType =/= cast orelse element(1, Event) =/= assign_delivery ->
    %% הודעה שקטה יותר - רק אם זה לא assign_delivery רגיל
    case {EventType, Event} of
        {info, {report_initial_state}} ->
            %% דיווח ראשוני על מצב השליח
            CourierId = maps:get(id, Data),
            report_state_change(CourierId, idle, #{
                delivered_packages => maps:get(delivered_packages, Data),
                total_delivered => maps:get(total_delivered, Data)
            });
        {info, _} ->
            io:format("Courier(~p) idle at delivery location, waiting for next assignment...~n", [maps:get(id, Data)]);
        _ ->
            ok
    end,
    {keep_state, Data};

handle_event(EventType, Event, StateName, Data) ->
    io:format("Courier(~p) in state ~p received unhandled event: ~p (~p)~n",
              [maps:get(id, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------

%% פונקציה עזר חדשה - זמן רנדומלי דינמי לפי ההגדרות
get_dynamic_travel_time() ->
    %% קריאת זמני הנסיעה מה-ETS
    case ets:info(simulation_config) of
        undefined ->
            %% אם אין הגדרות, השתמש בברירת מחדל
            rand:uniform(50001) + 9999;  % 10-60 שניות
        _ ->
            case ets:lookup(simulation_config, travel_times) of
                [{travel_times, MinTime, MaxTime}] ->
                    %% חישוב זמן רנדומלי בין Min ל-Max
                    Range = MaxTime - MinTime + 1,
                    MinTime + rand:uniform(Range) - 1;
                [] ->
                    %% אין הגדרות זמן, ברירת מחדל
                    rand:uniform(50001) + 9999
            end
    end.

%% דיווח על שינוי מצב למערכת הניטור
report_state_change(CourierId, NewStatus, AdditionalData) ->
    %% בדיקה אם State Collector פעיל
    case whereis(logistics_state_collector) of
        undefined ->
            %% אם State Collector לא פעיל, רק נדפיס הודעה
            io:format("DEBUG: State Collector not available for courier ~p state change~n", [CourierId]);
        _ ->
            %% שליחת עדכון ל-State Collector
            StateData = maps:merge(AdditionalData, #{status => NewStatus}),
            logistics_state_collector:courier_state_changed(CourierId, StateData)
    end.

%% -----------------------------------------------------------
%% דרישות gen_statem
%% -----------------------------------------------------------
terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.