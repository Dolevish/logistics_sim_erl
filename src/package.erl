%% -----------------------------------------------------------
%% מודול חבילה (Package) - FSM
%% כל תהליך חבילה מייצג משלוח יחיד
%% תיקון: הוספת מצב picking_up לדיווח סטטוס מדויק
%% -----------------------------------------------------------

-module(package).
-behaviour(gen_statem).

%% ממשק API
-export([start_link/2, assign_courier/2, update_status/2]).

%% Callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% >> תיקון 2: שינוי בחתימת הפונקציה <<
start_link(PkgId, Zone) ->
    Name = list_to_atom("package_" ++ PkgId),
    gen_statem:start_link({via, global, Name}, ?MODULE, [PkgId, Zone], []).

callback_mode() -> handle_event_function.

init([PkgId, Zone]) ->
    io:format("Package ~p created for zone ~p: ordered state~n", [PkgId, Zone]),

    %% מאתרים את ה-PID של מנהל האזור לפי שמו
    ZoneManagerPid = list_to_atom("zone_manager_" ++ Zone),

    %% דיווח למערכת הניטור על יצירת חבילה חדשה
    report_state_change(PkgId, ordered, #{zone => Zone, created_at => erlang:system_time(second)}),

    {ok, ordered, #{id => PkgId, zone_manager => ZoneManagerPid, zone => Zone}}.

assign_courier(PkgId, Courier) ->
    io:format("API: Assigning courier ~p to package ~p~n", [Courier, PkgId]),
    gen_statem:cast(list_to_atom("package_" ++ PkgId), {assign_courier, Courier}).

update_status(PkgId, Status) ->
    io:format("API: Updating package ~p status to ~p~n", [PkgId, Status]),
    gen_statem:cast(list_to_atom("package_" ++ PkgId), {update_status, Status}).

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------

%% מצב ordered – ממתין להקצאת שליח
handle_event(cast, {assign_courier, Courier}, ordered, Data) ->
    PkgId = maps:get(id, Data),
    io:format("Package(~p) assigned to courier ~p~n", [PkgId, Courier]),
    Zone = maps:get(zone, Data),
    gen_statem:cast(list_to_atom("courier_" ++ Courier), {assign_delivery, PkgId, Zone}),
    report_state_change(PkgId, assigned, #{courier => Courier, zone => Zone}),
    {next_state, assigned, Data#{courier => Courier}};

%% מצב assigned - הוקצה שליח, ממתין לאיסוף
handle_event(cast, {update_status, picking_up}, assigned, Data) ->
    PkgId = maps:get(id, Data),
    io:format("Package(~p) is being picked up~n", [PkgId]),
    %% >> תיקון 2: דיווח על הסטטוס החדש picking_up <<
    report_state_change(PkgId, picking_up, #{
        courier => maps:get(courier, Data),
        zone => maps:get(zone, Data)
    }),
    %% >> תיקון 2: מעבר למצב החדש picking_up <<
    {next_state, picking_up, Data};

%% >> תיקון 2: הוספת טיפול במצב picking_up <<
%% מצב picking_up - השליח בדרך לאסוף את החבילה
handle_event(cast, {update_status, in_transit}, picking_up, Data) ->
    PkgId = maps:get(id, Data),
    io:format("Package(~p) is now in transit to customer~n", [PkgId]),
    %% דיווח על הסטטוס in_transit (או delivering)
    report_state_change(PkgId, in_transit, #{
        courier => maps:get(courier, Data),
        zone => maps:get(zone, Data)
    }),
    {next_state, in_transit, Data};

%% מצב in_transit - בדרך ללקוח
handle_event(cast, {update_status, delivered}, in_transit, Data) ->
    PkgId = maps:get(id, Data),
    io:format("Package(~p) delivered!~n", [PkgId]),
    ZoneManager = maps:get(zone_manager, Data),
    gen_statem:cast(ZoneManager, {package_delivered, PkgId, maps:get(courier, Data)}),
    report_state_change(PkgId, delivered, #{
        courier => maps:get(courier, Data),
        zone => maps:get(zone, Data),
        delivered_at => erlang:system_time(second)
    }),
    {next_state, delivered, Data};

%% טיפול בכשל
handle_event(cast, {update_status, failed}, _AnyState, Data) ->
    PkgId = maps:get(id, Data),
    io:format("Package(~p) delivery failed!~n", [PkgId]),
    report_state_change(PkgId, failed, #{
        courier => maps:get(courier, Data, null),
        zone => maps:get(zone, Data)
    }),
    {next_state, failed, Data};

%% טיפול בעדכוני סטטוס לא צפויים
handle_event(cast, {update_status, Status}, CurrentState, Data) ->
    io:format("Package(~p) unexpected status update ~p in state ~p~n",
             [maps:get(id, Data), Status, CurrentState]),
    {keep_state, Data};

%% מצב delivered - הסופי
handle_event(_EventType, _Event, delivered, Data) ->
    {keep_state, Data};

%% API לקבלת מצב החבילה
handle_event({call, From}, get_state, StateName, Data) ->
    {keep_state, Data, [{reply, From, StateName}]};

%% catch-all לאירועים לא מזוהים
handle_event(EventType, Event, StateName, Data) ->
    io:format("Package(~p) in state ~p received unhandled event: ~p (~p)~n",
              [maps:get(id, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------
report_state_change(PackageId, NewStatus, AdditionalData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            io:format("DEBUG: State Collector not available for package ~p state change~n", [PackageId]);
        _ ->
            StateData = maps:merge(AdditionalData, #{status => NewStatus}),
            logistics_state_collector:package_state_changed(PackageId, StateData)
    end.

terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.
