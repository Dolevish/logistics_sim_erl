%% -----------------------------------------------------------
%% מודול מנהל אזור (Zone Manager) - FSM
%% אחראי על חלוקת משלוחים ושליחים באזור מסוים
%% תיקון: שינוי ל-handle_event mode כדי לפתור את בעיית ה-cast
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
%% -----------------------------------------------------------
init([ZoneName]) ->
    io:format("Zone Manager ~p initializing...~n", [ZoneName]),
    %% עדכן את כל שליחי האזור!
    {ok, monitoring, #{
        zone => ZoneName,
        couriers => ["courier1", "courier2", "courier3", "courier4"],
        available_couriers => ["courier1", "courier2", "courier3", "courier4"],
        waiting_packages => []
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
    Avail = maps:get(available_couriers, Data, []),
    Waiting = maps:get(waiting_packages, Data, []),
    io:format(
        ">>> DEBUG: Available couriers: ~p (~p total), Waiting packages: ~p (~p total)~n",
        [Avail, length(Avail), Waiting, length(Waiting)]
    ).

%% -----------------------------------------------------------
%% handle_event - מטפל בכל האירועים במצב אחיד
%% -----------------------------------------------------------
handle_event(cast, {new_package, PackageId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p) received new package: ~p~n", [maps:get(zone, Data), PackageId]),
    Avail = maps:get(available_couriers, Data),
    case Avail of
        [Courier | Rest] ->
            io:format("Zone(~p) assigns package ~p to courier ~p~n", [maps:get(zone, Data), PackageId, Courier]),
            %% יצירת תהליך חבילה
            {ok, _Pid} = package:start_link(PackageId, self()),
            %% תיקון: בדיקה שהשליח באמת פנוי לפני הקצאה
            CourierPid = whereis(list_to_atom("courier_" ++ Courier)),
            case CourierPid of
                undefined ->
                    io:format("Zone(~p): Courier ~p not found, requeueing package ~p~n", 
                             [maps:get(zone, Data), Courier, PackageId]),
                    Waiting = maps:get(waiting_packages, Data),
                    NewData = Data#{waiting_packages => Waiting ++ [PackageId]},
                    {keep_state, NewData};
                _ ->
                    package:assign_courier(PackageId, Courier),
                    NewData = Data#{available_couriers => Rest},
                    debug_state(NewData),
                    {keep_state, NewData}
            end;
        [] ->
            io:format("Zone(~p): No available couriers, package ~p waits in queue~n", [maps:get(zone, Data), PackageId]),
            Waiting = maps:get(waiting_packages, Data),
            NewData = Data#{waiting_packages => Waiting ++ [PackageId]},
            debug_state(NewData),
            {keep_state, NewData}
    end;

handle_event(cast, {courier_available, CourierId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p): Courier ~p is now available~n", [maps:get(zone, Data), CourierId]),
    Waiting = maps:get(waiting_packages, Data),
    case Waiting of
        [Pkg | RestPkgs] ->
            io:format("Zone(~p): Assigns waiting package ~p to courier ~p~n", [maps:get(zone, Data), Pkg, CourierId]),
            %% תיקון: בדיקה אם תהליך החבילה כבר קיים ובאיזה מצב
            case whereis(list_to_atom("package_" ++ Pkg)) of
                undefined ->
                    %% אם תהליך החבילה לא קיים, ניצור אותו
                    {ok, _Pid} = package:start_link(Pkg, self()),
                    package:assign_courier(Pkg, CourierId);
                _Pid ->
                    %% התהליך כבר קיים - רק נקצה את השליח
                    package:assign_courier(Pkg, CourierId)
            end,
            NewData = Data#{waiting_packages => RestPkgs},
            debug_state(NewData),
            {keep_state, NewData};
        [] ->
            Avail = maps:get(available_couriers, Data),
            NewData = Data#{available_couriers => Avail ++ [CourierId]},
            debug_state(NewData),
            {keep_state, NewData}
    end;

handle_event(cast, {package_delivered, PackageId, CourierId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p): Package ~p delivered by courier ~p!~n", [maps:get(zone, Data), PackageId, CourierId]),
    {keep_state, Data};

%% תיקון: טיפול בכשל הקצאה - שליח תפוס
handle_event(cast, {assignment_failed, PackageId, CourierId}, monitoring, Data) ->
    debug_state(Data),
    io:format("Zone(~p): Assignment failed - courier ~p busy, requeueing package ~p~n", 
              [maps:get(zone, Data), CourierId, PackageId]),
    %% החזר את החבילה לתור ההמתנה
    Waiting = maps:get(waiting_packages, Data),
    NewData = Data#{waiting_packages => Waiting ++ [PackageId]},
    debug_state(NewData),
    {keep_state, NewData};

%% טיפול במצבים נוספים
handle_event(cast, {start_optimization}, monitoring, Data) ->
    io:format("Zone(~p): Starting optimization cycle~n", [maps:get(zone, Data)]),
    {next_state, optimizing, Data};

handle_event(cast, {overload_detected}, monitoring, Data) ->
    io:format("Zone(~p): Overload detected, entering emergency mode~n", [maps:get(zone, Data)]),
    {next_state, emergency_mode, Data};

%% מצב אופטימיזציה
handle_event(cast, {optimization_complete}, optimizing, Data) ->
    io:format("Zone(~p): Optimization complete, returning to monitoring~n", [maps:get(zone, Data)]),
    {next_state, monitoring, Data};

handle_event(cast, {new_package, PackageId}, optimizing, Data) ->
    io:format("Zone(~p): Package ~p queued during optimization~n", [maps:get(zone, Data), PackageId]),
    Waiting = maps:get(waiting_packages, Data),
    NewData = Data#{waiting_packages => Waiting ++ [PackageId]},
    {keep_state, NewData};

%% מצב חירום
handle_event(cast, {load_balanced}, emergency_mode, Data) ->
    io:format("Zone(~p): Load balanced, returning to normal operation~n", [maps:get(zone, Data)]),
    {next_state, monitoring, Data};

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

%% מצב איזון עומסים
handle_event(cast, {load_balance_complete}, load_balancing, Data) ->
    io:format("Zone(~p): Load balancing complete~n", [maps:get(zone, Data)]),
    {next_state, monitoring, Data};

handle_event(cast, {transfer_package, PackageId, ToZone}, load_balancing, Data) ->
    io:format("Zone(~p): Transferring package ~p to zone ~p~n", [maps:get(zone, Data), PackageId, ToZone]),
    Waiting = maps:get(waiting_packages, Data),
    NewWaiting = lists:delete(PackageId, Waiting),
    NewData = Data#{waiting_packages => NewWaiting},
    {keep_state, NewData};

%% catch-all לאירועים לא מזוהים
handle_event(EventType, Event, StateName, Data) ->
    debug_state(Data),
    io:format("Zone(~p) in state ~p received unhandled event: ~p (~p)~n", 
              [maps:get(zone, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%% -----------------------------------------------------------
%% דרישות gen_statem
%% -----------------------------------------------------------
terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.