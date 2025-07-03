%% -----------------------------------------------------------
%% מודול מעקב אחר מיקומי שליחים (Location Tracker)
%% מנהל את התנועה של שליחים על המפה בזמן אמת
%% -----------------------------------------------------------
-module(location_tracker).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_tracking/4, stop_tracking/1, get_courier_status/1]).
-export([update_all_positions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% כלול את קובץ ה-header עם הגדרות ה-records
-include("map_records.hrl").

%% רשומת מעקב אחר שליח
-record(tracking, {
    courier_id,          % מזהה השליח
    start_location,      % נקודת מוצא
    end_location,        % נקודת יעד
    route,              % המסלול (כרגע פשוט - קו ישר)
    total_distance,     % מרחק כולל
    traveled_distance,  % מרחק שנסע
    speed,              % מהירות נוכחית (מטר/שנייה)
    start_time,         % זמן התחלת הנסיעה
    estimated_arrival,  % זמן הגעה משוער
    status,             % moving | arrived
    update_callback     % פונקציה להפעיל בהגעה
}).

-record(state, {
    active_trackings = #{},  % מעקבים פעילים
    update_timer             % טיימר לעדכון תקופתי
}).

%% קבועים
-define(UPDATE_INTERVAL, 1000).  % עדכון כל שנייה
-define(BASE_SPEED, 11.11).      % מהירות בסיסית: 40 קמ"ש = 11.11 מ/ש
-define(SPEED_VARIATION, 0.2).   % וריאציה במהירות: ±20%

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% התחלת מעקב אחר שליח
%% CourierId - מזהה השליח
%% FromLocation - מיקום התחלה
%% ToLocation - מיקום יעד
%% Callback - פונקציה להפעיל בהגעה {M, F, A}
start_tracking(CourierId, FromLocation, ToLocation, Callback) ->
    gen_server:call(?MODULE, {start_tracking, CourierId, FromLocation, ToLocation, Callback}).

%% הפסקת מעקב אחר שליח
stop_tracking(CourierId) ->
    gen_server:cast(?MODULE, {stop_tracking, CourierId}).

%% קבלת סטטוס נוכחי של שליח
get_courier_status(CourierId) ->
    gen_server:call(?MODULE, {get_courier_status, CourierId}).

%% עדכון ידני של כל המיקומים
update_all_positions() ->
    gen_server:cast(?MODULE, update_all_positions).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Location Tracker starting...~n"),
    
    %% התחלת טיימר לעדכונים תקופתיים
    Timer = erlang:send_after(?UPDATE_INTERVAL, self(), update_positions),
    
    {ok, #state{update_timer = Timer}}.

%% התחלת מעקב חדש
handle_call({start_tracking, CourierId, FromLocationId, ToLocationId, Callback}, _From, State) ->
    io:format("Location Tracker: Starting tracking for courier ~p from ~p to ~p~n", 
              [CourierId, FromLocationId, ToLocationId]),
    
    %% קבלת מידע על הלוקיישנים
    case {map_server:get_location(FromLocationId), map_server:get_location(ToLocationId)} of
        {{ok, FromLoc}, {ok, ToLoc}} ->
            %% --- התיקון מתחיל כאן ---
            %% בדיקה אם מדובר באותו מיקום, וטיפול משופר במקרה זה.
            if FromLocationId == ToLocationId ->
                io:format("Location Tracker: Source and destination are the same. Triggering immediate arrival.~n"),
                
                %% יוצרים רשומת מעקב מלאה שמייצגת הגעה מיידית.
                %% זה מבטיח שכל הפונקציות שמצפות לרשומה יפעלו כשורה.
                ArrivalTracking = #tracking{
                    courier_id = CourierId,
                    start_location = FromLoc,
                    end_location = ToLoc,
                    route = {direct, FromLoc, ToLoc},
                    total_distance = 0,
                    traveled_distance = 0,
                    speed = 0,
                    start_time = erlang:system_time(second),
                    estimated_arrival = erlang:system_time(second),
                    status = arrived,
                    update_callback = Callback
                },
                
                %% מפעילים את לוגיקת ההגעה באופן ידני.
                %% זה יעדכן את המיקום הסופי של השליח ויפעיל את ה-callback.
                handle_arrival(ArrivalTracking),
                
                %% מחזירים זמן הגעה של 0 שניות.
                {reply, {ok, 0}, State};
            
            true ->
                %% הלוגיקה הרגילה לנסיעה עם מרחק.
                Distance = calculate_distance(FromLoc, ToLoc),
                Speed = calculate_speed(),
                EstimatedTime = Distance / Speed,
                
                %% יצירת רשומת מעקב
                Tracking = #tracking{
                    courier_id = CourierId,
                    start_location = FromLoc,
                    end_location = ToLoc,
                    route = {direct, FromLoc, ToLoc}, % כרגע מסלול ישיר
                    total_distance = Distance,
                    traveled_distance = 0,
                    speed = Speed,
                    start_time = erlang:system_time(second),
                    estimated_arrival = erlang:system_time(second) + round(EstimatedTime),
                    status = moving,
                    update_callback = Callback
                },
                
                %% שמירה ברשימת המעקבים
                NewTrackings = maps:put(CourierId, Tracking, State#state.active_trackings),
                
                %% עדכון מיקום ראשוני
                update_courier_position(Tracking),
                
                {reply, {ok, EstimatedTime}, State#state{active_trackings = NewTrackings}}
            end;
            %% --- סוף התיקון ---
            
        _ ->
            {reply, {error, invalid_locations}, State}
    end;

%% קבלת סטטוס שליח
handle_call({get_courier_status, CourierId}, _From, State) ->
    case maps:get(CourierId, State#state.active_trackings, undefined) of
        undefined ->
            {reply, {error, not_tracking}, State};
        Tracking ->
            Status = build_status_report(Tracking),
            {reply, {ok, Status}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% הפסקת מעקב
handle_cast({stop_tracking, CourierId}, State) ->
    io:format("Location Tracker: Stopping tracking for courier ~p~n", [CourierId]),
    NewTrackings = maps:remove(CourierId, State#state.active_trackings),
    {noreply, State#state{active_trackings = NewTrackings}};

%% עדכון ידני של כל המיקומים
handle_cast(update_all_positions, State) ->
    update_all_courier_positions(State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% עדכון תקופתי של מיקומים
handle_info(update_positions, State) ->
    %% עדכון כל המיקומים
    NewState = update_all_courier_positions(State),
    
    %% תזמון העדכון הבא
    Timer = erlang:send_after(?UPDATE_INTERVAL, self(), update_positions),
    
    {noreply, NewState#state{update_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% ביטול הטיימר
    case State#state.update_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    io:format("Location Tracker terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

%% חישוב מרחק בין שתי נקודות
calculate_distance(FromLoc, ToLoc) ->
    DX = FromLoc#location.x - ToLoc#location.x,
    DY = FromLoc#location.y - ToLoc#location.y,
    math:sqrt(DX*DX + DY*DY).

%% חישוב מהירות עם וריאציה רנדומלית
calculate_speed() ->
    %% מהירות בסיסית עם וריאציה של ±20%
    Variation = (rand:uniform() * 2 - 1) * ?SPEED_VARIATION,
    ?BASE_SPEED * (1 + Variation).

%% עדכון מיקומי כל השליחים
update_all_courier_positions(State) ->
    %% עבור על כל המעקבים הפעילים
    UpdatedTrackings = maps:fold(fun(CourierId, Tracking, Acc) ->
        case Tracking#tracking.status of
            moving ->
                %% חישוב התקדמות
                CurrentTime = erlang:system_time(second),
                ElapsedTime = CurrentTime - Tracking#tracking.start_time,
                TraveledDistance = min(Tracking#tracking.speed * ElapsedTime, 
                                     Tracking#tracking.total_distance),
                
                %% עדכון המעקב
                UpdatedTracking = Tracking#tracking{traveled_distance = TraveledDistance},
                
                %% בדיקה אם הגענו ליעד
                if TraveledDistance >= Tracking#tracking.total_distance ->
                    %% הגענו!
                    handle_arrival(UpdatedTracking),
                    %% הסר מהמעקב או סמן כ-arrived
                    maps:put(CourierId, UpdatedTracking#tracking{status = arrived}, Acc);
                true ->
                    %% עדכון מיקום
                    update_courier_position(UpdatedTracking),
                    maps:put(CourierId, UpdatedTracking, Acc)
                end;
            arrived ->
                %% כבר הגיע, לא צריך לעדכן
                maps:put(CourierId, Tracking, Acc)
        end
    end, #{}, State#state.active_trackings),
    
    State#state{active_trackings = UpdatedTrackings}.

%% עדכון מיקום שליח בודד
update_courier_position(Tracking) ->
    %% חישוב המיקום הנוכחי על המסלול
    Progress = case Tracking#tracking.total_distance of
        0 -> 1.0;
        0.0 -> 1.0;
        D when D > 0 -> min(1.0, Tracking#tracking.traveled_distance / D);
        _ -> 1.0
    end,
    
    %% חישוב קואורדינטות (אינטרפולציה לינארית)
    StartLoc = Tracking#tracking.start_location,
    EndLoc = Tracking#tracking.end_location,
    
    CurrentX = StartLoc#location.x + (EndLoc#location.x - StartLoc#location.x) * Progress,
    CurrentY = StartLoc#location.y + (EndLoc#location.y - StartLoc#location.y) * Progress,
    
    %% חישוב זמן הגעה משוער מעודכן
    RemainingDistance = max(0, Tracking#tracking.total_distance - Tracking#tracking.traveled_distance),
    ETA = case Tracking#tracking.speed of
        0 -> 0;
        0.0 -> 0;
        Speed when Speed > 0 -> round(RemainingDistance / Speed);
        _ -> 0
    end,
    
    %% הכנת נתוני המיקום
    PositionData = #{
        position => #{x => round(CurrentX), y => round(CurrentY)},
        destination => list_to_binary(EndLoc#location.id),
        destination_address => list_to_binary(EndLoc#location.address),
        progress => Progress,
        eta => ETA,
        speed => round(Tracking#tracking.speed * 3.6), % המרה לקמ"ש
        status => moving
    },
    
    %% עדכון בmap_server
    map_server:update_courier_position(Tracking#tracking.courier_id, PositionData).

%% טיפול בהגעה ליעד
handle_arrival(Tracking) ->
    io:format("Location Tracker: Courier ~p arrived at ~p!~n", 
              [Tracking#tracking.courier_id, (Tracking#tracking.end_location)#location.id]),
    
    %% עדכון מיקום סופי
    FinalPosition = #{
        position => #{
            x => (Tracking#tracking.end_location)#location.x, 
            y => (Tracking#tracking.end_location)#location.y
        },
        destination => list_to_binary((Tracking#tracking.end_location)#location.id),
        progress => 1.0,
        eta => 0,
        status => arrived
    },
    
    map_server:update_courier_position(Tracking#tracking.courier_id, FinalPosition),
    
    %% הפעלת ה-callback אם קיים
    case Tracking#tracking.update_callback of
        undefined -> ok;
        {M, F, A} -> 
            spawn(fun() -> apply(M, F, A) end);
        Fun when is_function(Fun) ->
            spawn(Fun)
    end.

%% בניית דוח סטטוס
build_status_report(Tracking) ->
    Progress = case Tracking#tracking.total_distance of
        0 -> 1.0;
        0.0 -> 1.0;
        D when D > 0 -> min(1.0, Tracking#tracking.traveled_distance / D);
        _ -> 1.0
    end,
    
    RemainingDistance = max(0, Tracking#tracking.total_distance - Tracking#tracking.traveled_distance),
    ETA = case Tracking#tracking.speed of
        0 -> 0;
        0.0 -> 0;
        Speed when Speed > 0 -> round(RemainingDistance / Speed);
        _ -> 0
    end,
    
    #{
        courier_id => Tracking#tracking.courier_id,
        from => (Tracking#tracking.start_location)#location.id,
        to => (Tracking#tracking.end_location)#location.id,
        total_distance => round(Tracking#tracking.total_distance),
        traveled_distance => round(Tracking#tracking.traveled_distance),
        progress => Progress,
        speed_kmh => round(Tracking#tracking.speed * 3.6),
        eta_seconds => ETA,
        status => Tracking#tracking.status
    }.