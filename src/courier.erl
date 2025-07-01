%% -----------------------------------------------------------
%% מודול שליח (Courier) - FSM
%% כל תהליך שליח מייצג שליח אמיתי במערכת
%% עם התקדמות אוטומטית בין המצבים (כולל דיליי רנדומלי)
%% עדכון: שליחים לא חוזרים לדפו - נשארים זמינים במקום המסירה
%% -----------------------------------------------------------

-module(courier).
-behaviour(gen_statem).

%% ממשק API
-export([start_link/1]).

%% Callbacks - שינוי ל-handle_event mode
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

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
    %% zone_manager קבוע (אפשר להרחיב בהמשך)
    {ok, idle, #{id => CourierId, zone_manager => list_to_atom("zone_manager_north")}}.

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

%% מצב idle – שליח ממתין לקבלת משלוח חדש
handle_event(cast, {assign_delivery, PackageId}, idle, Data) ->
    io:format("Courier(~p) received new assignment: package ~p - heading to restaurant!~n", [maps:get(id, Data), PackageId]),
    %% עדכון סטטוס חבילה
    package:update_status(PackageId, picking_up),
    %% זמן נסיעה למסעדה רנדומלי בין 10 ל-60 שניות
    PickupMs = rand_time(),
    io:format("Courier(~p) will arrive at restaurant for package ~p in ~p ms~n", [maps:get(id, Data), PackageId, PickupMs]),
    erlang:send_after(PickupMs, self(), pickup_complete),
    {next_state, picking_up, Data#{package => PackageId}};

%% תיקון: שליח תפוס לא יכול לקבל הקצאות נוספות
handle_event(cast, {assign_delivery, PackageId}, StateName, Data) when StateName =/= idle ->
    io:format("Courier(~p) BUSY in state ~p, rejecting package ~p~n", [maps:get(id, Data), StateName, PackageId]),
    %% החזר את החבילה ל-zone manager כ"failed assignment"
    ZoneManager = maps:get(zone_manager, Data),
    gen_statem:cast(ZoneManager, {assignment_failed, PackageId, maps:get(id, Data)}),
    {keep_state, Data};

%% מצב picking_up – שליח נוסע למסעדה לאיסוף
handle_event(info, pickup_complete, picking_up, Data) ->
    io:format("Courier(~p) arrived at restaurant, picking up package ~p!~n", [maps:get(id, Data), maps:get(package, Data)]),
    package:update_status(maps:get(package, Data), in_transit),
    %% זמן נסיעה ללקוח רנדומלי בין 10 ל-60 שניות
    DeliveryMs = rand_time(),
    io:format("Courier(~p) heading to customer with package ~p, ETA: ~p ms~n", [maps:get(id, Data), maps:get(package, Data), DeliveryMs]),
    erlang:send_after(DeliveryMs, self(), delivery_complete),
    {next_state, delivering, Data};

%% מצב delivering – שליח בדרכו ללקוח
handle_event(info, delivery_complete, delivering, Data) ->
    io:format("Courier(~p) delivered package ~p, now available for next delivery!~n", [maps:get(id, Data), maps:get(package, Data)]),
    package:update_status(maps:get(package, Data), delivered),
    %% תיקון: במקום לחזור לדפו, השליח זמין מיד למשלוח הבא
    %% עדכון אוטומטי של zone_manager שהוא פנוי וזמין
    gen_statem:cast(maps:get(zone_manager, Data), {courier_available, maps:get(id, Data)}),
    {next_state, idle, maps:remove(package, Data)};

%% מצב moving_zone – שמור לעתיד, מעבר אזורים
handle_event(EventType, Event, moving_zone, Data) ->
    io:format("Courier(~p) moving_zone: ~p (~p)~n", [maps:get(id, Data), Event, EventType]),
    {keep_state, Data};

%% catch-all לאירועים לא מזוהים - כולל הודעות debug
handle_event(EventType, Event, idle, Data) when EventType =/= cast orelse element(1, Event) =/= assign_delivery ->
    %% הודעה שקטה יותר - רק אם זה לא assign_delivery רגיל
    case {EventType, Event} of
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
%% פונקציה עזר - זמן רנדומלי בין 10 שניות ל-60 שניות
%% -----------------------------------------------------------
rand_time() ->
    %% ערך רנדומלי בין 10_000 ל-60_000 מילי-שניות
    (rand:uniform(50_001) + 9_999).

%% -----------------------------------------------------------
%% דרישות gen_statem
%% -----------------------------------------------------------
terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.

