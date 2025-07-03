%% -----------------------------------------------------------
%% מודול שליח (Courier) משופר - עם תמיכה במיקומים וניווט
%% כל תהליך שליח מייצג שליח אמיתי במערכת
%% עם התקדמות אוטומטית בין המצבים וזמני נסיעה מבוססי מפה
%% -----------------------------------------------------------

-module(courier).
-behaviour(gen_statem).

%% ממשק API
-export([start_link/1]).
% הוספת פונקציות להשהיה והמשך
-export([pause/1, resume/1]).

%% Callbacks - שינוי ל-handle_event mode
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% כלול את קובץ ה-header עם הגדרות ה-records
-include("map_records.hrl").

%% הגדרת האזורים הקבועים
-define(FIXED_ZONES, ["north", "center", "south"]).

%% -----------------------------------------------------------
%% יצירת שליח חדש (כולל שם דינמי לפי מזהה)
%% -----------------------------------------------------------
start_link(CourierId) ->
    gen_statem:start_link({local, list_to_atom("courier_" ++ CourierId)}, ?MODULE, [CourierId], []).

% הוספת פונקציות API להשהיה והמשך
pause(CourierId) ->
    gen_statem:cast(list_to_atom("courier_" ++ CourierId), pause).

resume(CourierId) ->
    gen_statem:cast(list_to_atom("courier_" ++ CourierId), resume).

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
        total_delivered => 0,
        paused => false, % הוספת משתנה למצב השהיה
        current_location => undefined, % מיקום נוכחי
        home_base => undefined % בסיס הבית של השליח
    }}.

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

% טיפול בהשהיה
handle_event(cast, pause, StateName, Data) ->
    io:format("Courier ~p paused in state ~p~n", [maps:get(id, Data), StateName]),
    {keep_state, Data#{paused => true}};

% טיפול בהמשך
handle_event(cast, resume, StateName, Data) ->
    io:format("Courier ~p resumed in state ~p~n", [maps:get(id, Data), StateName]),
    {keep_state, Data#{paused => false}};


%% מצב idle – שליח ממתין לקבלת משלוח חדש
handle_event(cast, {assign_delivery, PackageId}, idle, Data) ->
    case maps:get(paused, Data) of
        true -> {keep_state, Data};
        false ->
            CourierId = maps:get(id, Data),
            io:format("Courier(~p) received new assignment: package ~p - heading to restaurant!~n", [CourierId, PackageId]),
            
            %% בדיקה אם המפה מופעלת
            MapEnabled = case ets:info(simulation_config) of
                undefined -> false;
                _ ->
                    case ets:lookup(simulation_config, map_enabled) of
                        [{map_enabled, true}] -> true;
                        _ -> false
                    end
            end,
            
            case MapEnabled of
                true ->
                    %% קבלת מידע על החבילה והיעדים
                    case get_package_locations(PackageId, Data) of
                        {ok, BusinessLocation, HomeLocation} ->
                            %% עדכון סטטוס חבילה
                            package:update_status(PackageId, picking_up),
                            
                            %% קביעת מיקום נוכחי (אם אין, השתמש במיקום העסק של האזור)
                            CurrentLocation = case maps:get(current_location, Data) of
                                undefined -> BusinessLocation;
                                Loc -> Loc
                            end,
                            
                            %% התחלת מעקב אחר התנועה לעסק
                            StartCallback = fun() -> self() ! pickup_complete end,
                            case location_tracker:start_tracking(CourierId, CurrentLocation, BusinessLocation, StartCallback) of
                                {ok, EstimatedTime} ->
                                    io:format("Courier(~p) will arrive at restaurant for package ~p in ~p seconds~n", 
                                            [CourierId, PackageId, round(EstimatedTime)]),
                                    
                                    %% דיווח למערכת הניטור על שינוי המצב
                                    report_state_change(CourierId, picking_up, #{
                                        package => PackageId, 
                                        eta => round(EstimatedTime * 1000),
                                        destination => BusinessLocation
                                    }),
                                    
                                    {next_state, picking_up, Data#{
                                        package => PackageId,
                                        business_location => BusinessLocation,
                                        home_location => HomeLocation,
                                        current_location => CurrentLocation
                                    }};
                                Error ->
                                    io:format("Courier(~p) failed to start tracking: ~p~n", [CourierId, Error]),
                                    {keep_state, Data}
                            end;
                        {error, Reason} ->
                            io:format("Courier(~p) failed to get package locations: ~p~n", [CourierId, Reason]),
                            %% נפול בחזרה לשיטה הישנה
                            handle_delivery_without_map(CourierId, PackageId, Data)
                    end;
                false ->
                    %% המפה לא מופעלת - השתמש בזמנים רנדומליים
                    handle_delivery_without_map(CourierId, PackageId, Data)
            end
    end;

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
    case maps:get(paused, Data) of
        true ->
            erlang:send_after(1000, self(), pickup_complete), % נסה שוב מאוחר יותר
            {keep_state, Data};
        false ->
            CourierId = maps:get(id, Data),
            PackageId = maps:get(package, Data),
            
            io:format("Courier(~p) arrived at restaurant, picking up package ~p!~n", [CourierId, PackageId]),
            package:update_status(PackageId, in_transit),
            
            %% בדיקה אם המפה מופעלת
            MapEnabled = case ets:info(simulation_config) of
                undefined -> false;
                _ ->
                    case ets:lookup(simulation_config, map_enabled) of
                        [{map_enabled, true}] -> true;
                        _ -> false
                    end
            end,
            
            case MapEnabled of
                true ->
                    BusinessLocation = maps:get(business_location, Data),
                    HomeLocation = maps:get(home_location, Data),
                    
                    %% עצירת המעקב הקודם
                    location_tracker:stop_tracking(CourierId),
                    
                    %% התחלת מעקב חדש מהעסק לבית הלקוח
                    DeliveryCallback = fun() -> self() ! delivery_complete end,
                    case location_tracker:start_tracking(CourierId, BusinessLocation, HomeLocation, DeliveryCallback) of
                        {ok, EstimatedTime} ->
                            io:format("Courier(~p) heading to customer with package ~p, ETA: ~p seconds~n", 
                                    [CourierId, PackageId, round(EstimatedTime)]),
                            
                            %% דיווח למערכת הניטור
                            Zone = maps:get(zone, Data, "unknown"),
                            report_state_change(CourierId, delivering, #{
                                package => PackageId, 
                                zone => Zone, 
                                eta => round(EstimatedTime * 1000),
                                destination => HomeLocation
                            }),
                            
                            {next_state, delivering, Data#{current_location => BusinessLocation}};
                        Error ->
                            io:format("Courier(~p) failed to start delivery tracking: ~p~n", [CourierId, Error]),
                            %% נפול בחזרה לשיטה הישנה
                            handle_pickup_complete_without_map(CourierId, PackageId, Data)
                    end;
                false ->
                    %% המפה לא מופעלת - השתמש בזמנים רנדומליים
                    handle_pickup_complete_without_map(CourierId, PackageId, Data)
            end
    end;

%% מצב delivering – שליח בדרכו ללקוח
handle_event(info, delivery_complete, delivering, Data) ->
    case maps:get(paused, Data) of
        true ->
            erlang:send_after(1000, self(), delivery_complete), % נסה שוב מאוחר יותר
            {keep_state, Data};
        false ->
            CourierId = maps:get(id, Data),
            PackageId = maps:get(package, Data),
            HomeLocation = maps:get(home_location, Data, undefined),
            
            io:format("Courier(~p) delivered package ~p, now available for next delivery!~n", [CourierId, PackageId]),
            package:update_status(PackageId, delivered),
            
            %% עצירת המעקב אם המפה מופעלת
            MapEnabled = case ets:info(simulation_config) of
                undefined -> false;
                _ ->
                    case ets:lookup(simulation_config, map_enabled) of
                        [{map_enabled, true}] -> true;
                        _ -> false
                    end
            end,
            
            case MapEnabled of
                true -> location_tracker:stop_tracking(CourierId);
                false -> ok
            end,

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
                zone => null,
                current_location => HomeLocation, % השליח נשאר במיקום הלקוח האחרון
                business_location => undefined,
                home_location => undefined
            }),

            {next_state, idle, NewData}
    end;

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

%% קבלת מיקומי העסק והבית עבור חבילה
get_package_locations(PackageId, _Data) ->
    %% חילוץ האזור מה-ID של החבילה
    case string:tokens(PackageId, "_") of
        [Zone | _] ->
            %% קבלת העסק באזור
            case map_server:get_business_in_zone(list_to_atom(Zone)) of
                {ok, Business} ->
                    %% קבלת בית רנדומלי באזור (בעתיד נקבל את הבית הספציפי)
                    case map_server:get_random_home_in_zone(list_to_atom(Zone)) of
                        {ok, Home} ->
                            {ok, Business#location.id, Home#location.id};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, invalid_package_id}
    end.

%% טיפול במשלוח ללא מפה (זמנים רנדומליים)
handle_delivery_without_map(CourierId, PackageId, Data) ->
    io:format("Courier(~p) handling delivery without map for package ~p~n", [CourierId, PackageId]),
    
    %% עדכון סטטוס חבילה
    package:update_status(PackageId, picking_up),
    
    %% זמן נסיעה למסעדה רנדומלי לפי ההגדרות
    PickupMs = get_dynamic_travel_time(),
    io:format("Courier(~p) will arrive at restaurant for package ~p in ~p ms~n", [CourierId, PackageId, PickupMs]),
    
    %% דיווח למערכת הניטור על שינוי המצב
    report_state_change(CourierId, picking_up, #{package => PackageId, eta => PickupMs}),
    
    erlang:send_after(PickupMs, self(), pickup_complete),
    {next_state, picking_up, Data#{package => PackageId}}.

%% טיפול בהשלמת איסוף ללא מפה
handle_pickup_complete_without_map(CourierId, PackageId, Data) ->
    %% זמן נסיעה ללקוח רנדומלי לפי ההגדרות
    DeliveryMs = get_dynamic_travel_time(),
    io:format("Courier(~p) heading to customer with package ~p, ETA: ~p ms~n", [CourierId, PackageId, DeliveryMs]),
    
    %% דיווח למערכת הניטור
    Zone = maps:get(zone, Data, "unknown"),
    report_state_change(CourierId, delivering, #{package => PackageId, zone => Zone, eta => DeliveryMs}),
    
    erlang:send_after(DeliveryMs, self(), delivery_complete),
    {next_state, delivering, Data}.

%% פונקציה עזר - זמן רנדומלי דינמי לפי ההגדרות
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