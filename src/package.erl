%% -----------------------------------------------------------
%% מודול חבילה (Package) - FSM
%% כל תהליך חבילה מייצג משלוח יחיד
%% תיקון: שמירת האזור המקורי והעברתו לשליח
%% -----------------------------------------------------------

-module(package).
-behaviour(gen_statem).

%% ממשק API - הוספתי exports לכל הפונקציות הציבוריות
-export([start_link/2, assign_courier/2, update_status/2]).

%% Callbacks - שינוי ל-handle_event mode
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

start_link(PkgId, ZoneManager) ->
    gen_statem:start_link({local, list_to_atom("package_" ++ PkgId)}, ?MODULE, [PkgId, ZoneManager], []).

%% תיקון: שינוי ל-handle_event mode
callback_mode() -> handle_event_function.

init([PkgId, ZoneManager]) ->
    io:format("Package ~p created: ordered state~n", [PkgId]),
    %% תיקון: שמירת האזור המקורי של החבילה
    Zone = case ZoneManager of
        Atom when is_atom(Atom) ->
            %% חילוץ שם האזור מהאטום zone_manager_XXX
            AtomStr = atom_to_list(Atom),
            case string:split(AtomStr, "zone_manager_") of
                ["", ZoneName] -> ZoneName;
                _ -> "unknown"
            end;
        _ -> "unknown"
    end,
    {ok, ordered, #{id => PkgId, zone_manager => ZoneManager, zone => Zone}}.

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
    io:format("Package(~p) assigned to courier ~p~n", [maps:get(id, Data), Courier]),
    %% תיקון: שליחת האזור המקורי לשליח
    Zone = maps:get(zone, Data),
    gen_statem:cast(list_to_atom("courier_" ++ Courier), {assign_delivery, maps:get(id, Data), Zone}),
    {next_state, assigned, Data#{courier => Courier}};

%% תיקון: חבילה שכבר הוקצתה לא יכולה להיות מוקצית שוב
handle_event(cast, {assign_courier, Courier}, assigned, Data) ->
    ExistingCourier = maps:get(courier, Data, none),
    io:format("Package(~p) already assigned to ~p, rejecting new assignment to ~p~n", 
              [maps:get(id, Data), ExistingCourier, Courier]),
    {keep_state, Data};

%% מצב assigned - הוקצה שליח, ממתין לאיסוף
handle_event(cast, {update_status, picking_up}, assigned, Data) ->
    io:format("Package(~p) is being picked up~n", [maps:get(id, Data)]),
    {next_state, in_transit, Data};

%% מצב in_transit - בדרך ללקוח
handle_event(cast, {update_status, delivered}, in_transit, Data) ->
    io:format("Package(~p) delivered!~n", [maps:get(id, Data)]),
    %% עדכון zone_manager שהחבילה נמסרה
    ZoneManagerAtom = maps:get(zone_manager, Data),
    gen_statem:cast(ZoneManagerAtom, {package_delivered, maps:get(id, Data), maps:get(courier, Data)}),
    {next_state, delivered, Data};

handle_event(cast, {update_status, failed}, in_transit, Data) ->
    io:format("Package(~p) delivery failed!~n", [maps:get(id, Data)]),
    {next_state, failed, Data};

%% תיקון: מניעת update status כפול - אם החבילה כבר במצב נכון, התעלם
handle_event(cast, {update_status, Status}, CurrentState, Data) ->
    %% המרת שמות מצבים לבדיקה
    ExpectedState = case Status of
        picking_up -> in_transit;
        in_transit -> in_transit;
        delivered -> delivered;
        failed -> failed;
        _ -> undefined
    end,
    
    if 
        CurrentState == ExpectedState ->
            io:format("Package(~p) already in correct state ~p, ignoring duplicate status update ~p~n", 
                     [maps:get(id, Data), CurrentState, Status]),
            {keep_state, Data};
        true ->
            io:format("Package(~p) unexpected status update ~p in state ~p~n", 
                     [maps:get(id, Data), Status, CurrentState]),
            {keep_state, Data}
    end;

%% מצב failed - נכשל, יכול להיות מוקצה מחדש
handle_event(cast, {assign_courier, Courier}, failed, Data) ->
    io:format("Package(~p) reassigned to courier ~p after failure~n", [maps:get(id, Data), Courier]),
    %% תיקון: שליחת האזור המקורי לשליח
    Zone = maps:get(zone, Data),
    gen_statem:cast(list_to_atom("courier_" ++ Courier), {assign_delivery, maps:get(id, Data), Zone}),
    {next_state, assigned, Data#{courier => Courier}};

handle_event(cast, {cancel}, failed, Data) ->
    io:format("Package(~p) cancelled after failure~n", [maps:get(id, Data)]),
    {keep_state, Data#{status => cancelled}};

%% מצב delivered - הסופי
handle_event(cast, Event, delivered, Data) ->
    io:format("Package(~p) delivered (final): ~p~n", [maps:get(id, Data), Event]),
    {keep_state, Data};

%% API לקבלת מצב החבילה
handle_event({call, From}, get_state, StateName, Data) ->
    {keep_state, Data, [{reply, From, StateName}]};

%% catch-all לאירועים לא מזוהים
handle_event(EventType, Event, StateName, Data) ->
    io:format("Package(~p) in state ~p received unhandled event: ~p (~p)~n", 
              [maps:get(id, Data), StateName, Event, EventType]),
    {keep_state, Data}.

terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.