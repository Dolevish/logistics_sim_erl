%% -----------------------------------------------------------
%% מודול שליח (Courier) משופר - עם תמיכה במיקומים וניווט
%% כל תהליך שליח מייצג שליח אמיתי במערכת
%% עם התקדמות אוטומטית בין המצבים וזמני נסיעה מבוססי מפה
%% -- גרסה משודרגת עם ניתוב מבוסס גרף --
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

    %% @@ הערה בעברית: התיקון - דיווח מיידי על המצב הראשוני @@
    %% @@ במקום לשלוח הודעה מושהית לעצמו, אנו קוראים ישירות לפונקציית הדיווח. @@
    %% @@ זה מבטיח שהמצב "idle" מדווח לפני כל מצב אחר שיכול להגיע. @@
    report_state_change(CourierId, idle, #{
        delivered_packages => [],
        total_delivered => 0
    }),

    %% הגדרת מיקום התחלתי אקראי לשליח כדי למנוע תקיעות במשימה הראשונה.
    %% קוראים לשרת המפה כדי לקבל נקודה אקראית על המפה.
    {InitialLocation, HomeBase} = case map_server:get_random_location() of
        {ok, Loc} ->
            io:format("Courier ~p initialized at random location: ~p~n", [CourierId, Loc#location.id]),
            {Loc#location.id, Loc#location.id};
        {error, Reason} ->
            %% אם אין אפשרות לקבל מיקום מהמפה (למקרה שהמפה עוד לא אותחלה),
            %% נשתמש בערך ברירת מחדל 'undefined' ונקווה לטוב.
            io:format("Error: Courier ~p could not get initial location: ~p~n", [CourierId, Reason]),
            {undefined, undefined}
    end,

    {ok, idle, #{
        id => CourierId,
        zones => ?FIXED_ZONES,  %% שימוש באזורים הקבועים
        delivered_packages => [],
        total_delivered => 0,
        paused => false, % הוספת משתנה למצב השהיה
        current_location => InitialLocation, % מיקום נוכחי
        home_base => HomeBase, % בסיס הבית של השליח
        %% >> הערה חדשה: הוספת שדה לאירועים שממתינים לעיבוד <<
        pending_event => undefined
    }}.

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

% טיפול בהשהיה
handle_event(cast, pause, StateName, Data) ->
    io:format("Courier ~p paused in state ~p~n", [maps:get(id, Data), StateName]),
    {keep_state, Data#{paused => true}};

% >> הערה חדשה: שינוי הטיפול בהמשך הסימולציה <<
handle_event(cast, resume, StateName, Data) ->
    io:format("Courier ~p resumed in state ~p~n", [maps:get(id, Data), StateName]),
    NewData = Data#{paused => false},
    % בדיקה אם יש אירוע שממתין
    case maps:get(pending_event, NewData) of
        undefined ->
            {keep_state, NewData};
        PendingEvent ->
            % אם כן, שולחים אותו לעצמנו לעיבוד מחדש
            self() ! PendingEvent,
            {keep_state, NewData#{pending_event => undefined}}
    end;


%% --- שינוי באופן הטיפול במשימה חדשה ---
%% מצב idle – שליח ממתין לקבלת משלוח חדש
%% כעת מקבלים גם את PackageId וגם את FromZone
handle_event(cast, {assign_delivery, PackageId, FromZone}, idle, Data) ->
    case maps:get(paused, Data) of
        true -> {keep_state, Data};
        false ->
            CourierId = maps:get(id, Data),
            io:format("Courier(~p) received new assignment: package ~p from zone ~p~n", [CourierId, PackageId, FromZone]),

            CurrentLocation = maps:get(current_location, Data),
            case get_package_locations(PackageId, FromZone) of
                {ok, BusinessLocation, HomeLocation} ->
                    package:update_status(PackageId, picking_up),

                    CourierPid = self(),
                    StartCallback = fun() -> CourierPid ! pickup_complete end,

                    case location_tracker:start_tracking(CourierId, CurrentLocation, BusinessLocation, StartCallback) of
                        {ok, EstimatedTime} ->
                            io:format("Courier(~p) will arrive at restaurant for package ~p in ~p seconds~n",
                                    [CourierId, PackageId, round(EstimatedTime)]),

                            report_state_change(CourierId, picking_up, #{
                                package => PackageId,
                                eta => round(EstimatedTime * 1000),
                                destination => BusinessLocation
                            }),

                            {next_state, picking_up, Data#{
                                package => PackageId,
                                zone => FromZone,
                                business_location => BusinessLocation,
                                home_location => HomeLocation
                            }};
                        Error ->
                            io:format("Courier(~p) failed to start tracking: ~p~n", [CourierId, Error]),
                            {keep_state, Data}
                    end;
                {error, Reason} ->
                    io:format("Courier(~p) failed to get package locations: ~p~n", [CourierId, Reason]),
                    {keep_state, Data}
            end
    end;

handle_event(cast, {assign_delivery, _PackageId, FromZone}, StateName, Data) when StateName =/= idle ->
    io:format("Courier(~p) BUSY in state ~p, rejecting package from zone ~p~n",
              [maps:get(id, Data), StateName, FromZone]),
    ZoneManager = list_to_atom("zone_manager_" ++ FromZone),
    case whereis(ZoneManager) of
        undefined -> ok;
        _ -> gen_statem:cast(ZoneManager, {assignment_failed, maps:get(package, Data), maps:get(id, Data)})
    end,
    {keep_state, Data};

%% >> הערה חדשה: שינוי הטיפול בסיום האיסוף כדי להתמודד עם מצב השהיה <<
handle_event(info, pickup_complete, picking_up, Data) ->
    case maps:get(paused, Data) of
        true ->
            % אם מושהה, שומרים את האירוע לעיבוד מאוחר יותר
            {keep_state, Data#{pending_event => pickup_complete}};
        false ->
            CourierId = maps:get(id, Data),
            PackageId = maps:get(package, Data),

            io:format("Courier(~p) arrived at restaurant, picking up package ~p!~n", [CourierId, PackageId]),
            package:update_status(PackageId, in_transit),

            BusinessLocation = maps:get(business_location, Data),
            HomeLocation = maps:get(home_location, Data),

            location_tracker:stop_tracking(CourierId),

            CourierPid = self(),
            DeliveryCallback = fun() -> CourierPid ! delivery_complete end,

            case location_tracker:start_tracking(CourierId, BusinessLocation, HomeLocation, DeliveryCallback) of
                {ok, EstimatedTime} ->
                    io:format("Courier(~p) heading to customer with package ~p, ETA: ~p seconds~n",
                            [CourierId, PackageId, round(EstimatedTime)]),

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
                    {keep_state, Data}
            end
    end;

%% >> הערה חדשה: שינוי הטיפול בסיום המשלוח כדי להתמודד עם מצב השהיה <<
handle_event(info, delivery_complete, delivering, Data) ->
    case maps:get(paused, Data) of
        true ->
            % אם מושהה, שומרים את האירוע לעיבוד מאוחר יותר
            {keep_state, Data#{pending_event => delivery_complete}};
        false ->
            CourierId = maps:get(id, Data),
            PackageId = maps:get(package, Data),
            HomeLocation = maps:get(home_location, Data, undefined),

            io:format("Courier(~p) delivered package ~p, now available for next delivery!~n", [CourierId, PackageId]),
            package:update_status(PackageId, delivered),

            location_tracker:stop_tracking(CourierId),

            DeliveredPackages = maps:get(delivered_packages, Data),
            TotalDelivered = maps:get(total_delivered, Data),
            NewDeliveredPackages = [PackageId | DeliveredPackages],
            NewTotalDelivered = TotalDelivered + 1,

            report_state_change(CourierId, idle, #{
                delivered_packages => NewDeliveredPackages,
                total_delivered => NewTotalDelivered
            }),

            courier_pool:return_courier(CourierId),

            NewData = maps:remove(package, Data#{
                delivered_packages => NewDeliveredPackages,
                total_delivered => NewTotalDelivered,
                zone => null,
                current_location => HomeLocation, %% <- המיקום הנוכחי מתעדכן למיקום המסירה
                business_location => undefined,
                home_location => undefined
            }),

            {next_state, idle, NewData}
    end;

%% מצב moving_zone – שמור לעתיד, מעבר אזורים
handle_event(EventType, Event, moving_zone, Data) ->
    io:format("Courier(~p) moving_zone: ~p (~p)~n", [maps:get(id, Data), Event, EventType]),
    {keep_state, Data};

%% @@ הערה בעברית: הקוד שטיפל ב-report_initial_state נמחק, @@
%% @@ מכיוון שהדיווח מתבצע כעת באופן מיידי בפונקציית init. @@
handle_event(cast, _, idle, Data) ->
    {keep_state, Data};

handle_event(info, _, idle, Data) ->
    io:format("Courier(~p) idle at delivery location, waiting for next assignment...~n", [maps:get(id, Data)]),
    {keep_state, Data};

handle_event(EventType, Event, StateName, Data) ->
    io:format("Courier(~p) in state ~p received unhandled event: ~p (~p)~n",
              [maps:get(id, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------

%% --- שינוי: הפונקציה מקבלת גם את האזור כדי למנוע בלבול ---
get_package_locations(PackageId, Zone) ->
    io:format("Courier: Getting locations for package ~p in zone ~p~n", [PackageId, Zone]),
    Tokens = string:tokens(PackageId, "_"),
    case find_home_pattern(Tokens) of
        {ok, _ZoneFromId, HomeId} ->
            io:format("Courier: Found home ~p in package ID for zone ~p~n", [HomeId, Zone]),
            case map_server:get_business_in_zone(list_to_atom(Zone)) of
                {ok, Business} ->
                    io:format("Courier: Will deliver from ~p to ~p~n", [Business#location.id, HomeId]),
                    {ok, Business#location.id, HomeId};
                Error ->
                    io:format("Courier: Failed to get business for zone ~p: ~p~n", [Zone, Error]),
                    Error
            end;
        {error, _} ->
            io:format("Courier: Could not find home in package ID, using random home in zone ~p~n", [Zone]),
            case map_server:get_business_in_zone(list_to_atom(Zone)) of
                {ok, Business} ->
                    case map_server:get_random_home_in_zone(list_to_atom(Zone)) of
                        {ok, Home} -> {ok, Business#location.id, Home#location.id};
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

find_home_pattern([Zone | Rest]) ->
    case find_to_home_in_list(Rest) of
        {ok, HomeId} -> {ok, Zone, HomeId};
        error -> {error, not_found}
    end;
find_home_pattern(_) ->
    {error, invalid_format}.

find_to_home_in_list(["to" | Rest]) ->
    case Rest of
        ["home", NumStr | _] ->
            {ok, "home_" ++ NumStr};
        [HomeIdWithPrefix | _] ->
            case string:prefix(HomeIdWithPrefix, "home") of
                nomatch -> error;
                _ -> {ok, HomeIdWithPrefix}
            end;
        _ ->
            error
    end;
find_to_home_in_list([_ | Rest]) ->
    find_to_home_in_list(Rest);
find_to_home_in_list([]) ->
    error.

report_state_change(CourierId, NewStatus, AdditionalData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            io:format("DEBUG: State Collector not available for courier ~p state change~n", [CourierId]);
        _ ->
            StateData = maps:merge(AdditionalData, #{status => NewStatus}),
            logistics_state_collector:courier_state_changed(CourierId, StateData)
    end.

%% -----------------------------------------------------------
%% דרישות gen_statem
%% -----------------------------------------------------------
terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.