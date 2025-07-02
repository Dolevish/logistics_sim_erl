%% -----------------------------------------------------------
%% מודול איסוף מצב המערכת (State Collector) - מתוקן
%% אוסף מידע מכל הרכיבים במערכת ומפיץ עדכונים ל-WebSocket handlers
%% משמש כ-Event Bus מרכזי לעדכוני UI
%% -----------------------------------------------------------
-module(logistics_state_collector).
-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, unsubscribe/1]).
-export([courier_state_changed/2, package_state_changed/2, zone_state_changed/2]).
-export([get_courier_info/1, get_package_info/1, get_zone_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

%% התחלת ה-State Collector
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% רישום handler לקבלת עדכונים
subscribe(HandlerPid) ->
    gen_server:cast(?MODULE, {subscribe, HandlerPid}).

%% ביטול רישום של handler
unsubscribe(HandlerPid) ->
    gen_server:cast(?MODULE, {unsubscribe, HandlerPid}).

%% דיווח על שינוי במצב שליח
courier_state_changed(CourierId, NewState) ->
    gen_server:cast(?MODULE, {courier_update, CourierId, NewState}).

%% דיווח על שינוי במצב חבילה
package_state_changed(PackageId, NewState) ->
    gen_server:cast(?MODULE, {package_update, PackageId, NewState}).

%% דיווח על שינוי במצב אזור
zone_state_changed(Zone, NewState) ->
    gen_server:cast(?MODULE, {zone_update, Zone, NewState}).

%% קבלת מידע על שליח ספציפי
get_courier_info(CourierId) ->
    gen_server:call(?MODULE, {get_courier_info, CourierId}).

%% קבלת מידע על חבילה ספציפית
get_package_info(PackageId) ->
    gen_server:call(?MODULE, {get_package_info, PackageId}).

%% קבלת מידע על אזור ספציפי
get_zone_info(Zone) ->
    gen_server:call(?MODULE, {get_zone_info, Zone}).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Logistics State Collector starting...~n"),
    
    %% טבלאות ETS לשמירת מצב - בדיקה אם כבר קיימות
    case ets:info(courier_states) of
        undefined -> ets:new(courier_states, [named_table, public, {keypos, 1}]);
        _ -> ok
    end,
    case ets:info(package_states) of
        undefined -> ets:new(package_states, [named_table, public, {keypos, 1}]);
        _ -> ok
    end,
    case ets:info(zone_states) of
        undefined -> ets:new(zone_states, [named_table, public, {keypos, 1}]);
        _ -> ok
    end,
    
    %% אתחול מצב השליחים
    init_courier_states(),
    
    %% תזמון בדיקה תקופתית של מצב המערכת
    erlang:send_after(5000, self(), check_system_state),
    
    {ok, #{
        subscribers => [],  %% רשימת WebSocket handlers רשומים
        update_counter => 0  %% מונה עדכונים
    }}.

%% טיפול ב-subscribe
handle_cast({subscribe, HandlerPid}, State) ->
    io:format("State Collector: New subscriber ~p~n", [HandlerPid]),
    Subscribers = maps:get(subscribers, State),
    
    %% מוניטור על ה-handler כדי לזהות ניתוק
    erlang:monitor(process, HandlerPid),
    
    NewState = State#{subscribers => [HandlerPid | Subscribers]},
    {noreply, NewState};

%% טיפול ב-unsubscribe
handle_cast({unsubscribe, HandlerPid}, State) ->
    io:format("State Collector: Unsubscribing ~p~n", [HandlerPid]),
    Subscribers = maps:get(subscribers, State),
    NewState = State#{subscribers => lists:delete(HandlerPid, Subscribers)},
    {noreply, NewState};

%% טיפול בעדכון מצב שליח - מתוקן לשמור נכון את total_delivered
handle_cast({courier_update, CourierId, NewState}, State) ->
    io:format("State Collector: Courier ~p state changed to ~p~n", [CourierId, NewState]),
    
    %% קבלת המידע הקיים
    ExistingInfo = case ets:lookup(courier_states, CourierId) of
        [{_, Info}] -> Info;
        [] -> #{}
    end,
    
    %% בניית המידע המעודכן עם שמירה נכונה של total_delivered
    UpdatedInfo = build_courier_info(CourierId, NewState, ExistingInfo),
    
    %% שמירת המצב החדש ב-ETS
    ets:insert(courier_states, {CourierId, UpdatedInfo}),
    
    %% שליחת עדכון לכל המנויים
    broadcast_update(<<"courier_update">>, UpdatedInfo, State),
    
    %% עדכון המונה
    Counter = maps:get(update_counter, State),
    {noreply, State#{update_counter => Counter + 1}};

%% טיפול בעדכון מצב חבילה
handle_cast({package_update, PackageId, NewState}, State) ->
    io:format("State Collector: Package ~p state changed to ~p~n", [PackageId, NewState]),
    
    %% שמירת המצב החדש ב-ETS
    PackageInfo = build_package_info(PackageId, NewState),
    ets:insert(package_states, {PackageId, PackageInfo}),
    
    %% שליחת עדכון לכל המנויים
    broadcast_update(<<"package_update">>, PackageInfo, State),
    
    %% עדכון המונה
    Counter = maps:get(update_counter, State),
    {noreply, State#{update_counter => Counter + 1}};

%% טיפול בעדכון מצב אזור
handle_cast({zone_update, Zone, NewState}, State) ->
    io:format("State Collector: Zone ~p state changed~n", [Zone]),
    
    %% בדיקה שהטבלה קיימת
    case ets:info(zone_states) of
        undefined ->
            io:format("Warning: zone_states table not ready yet~n"),
            {noreply, State};
        _ ->
            %% שמירת המצב החדש ב-ETS
            ZoneInfo = build_zone_info(Zone, NewState),
            ets:insert(zone_states, {Zone, ZoneInfo}),
            
            %% שליחת עדכון לכל המנויים
            broadcast_update(<<"zone_update">>, ZoneInfo, State),
            
            %% עדכון המונה
            Counter = maps:get(update_counter, State),
            {noreply, State#{update_counter => Counter + 1}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% טיפול בבקשות מידע
handle_call({get_courier_info, CourierId}, _From, State) ->
    Reply = case ets:lookup(courier_states, CourierId) of
        [{_, Info}] -> {ok, Info};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get_package_info, PackageId}, _From, State) ->
    Reply = case ets:lookup(package_states, PackageId) of
        [{_, Info}] -> {ok, Info};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({get_zone_info, Zone}, _From, State) ->
    Reply = case ets:lookup(zone_states, Zone) of
        [{_, Info}] -> {ok, Info};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% בדיקה תקופתית של מצב המערכת
handle_info(check_system_state, State) ->
    io:format("State Collector: Performing periodic system check~n"),
    
    %% בדיקת מצב כל השליחים
    check_all_couriers(),
    
    %% בדיקת מצב כל האזורים
    check_all_zones(),
    
    %% תזמון הבדיקה הבאה
    erlang:send_after(5000, self(), check_system_state),
    
    {noreply, State};

%% טיפול בניתוק של subscriber
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    io:format("State Collector: Subscriber ~p disconnected~n", [Pid]),
    Subscribers = maps:get(subscribers, State),
    NewState = State#{subscribers => lists:delete(Pid, Subscribers)},
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("State Collector terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

%% אתחול מצב השליחים
init_courier_states() ->
    CourierIds = ["courier1", "courier2", "courier3", "courier4", 
                  "courier5", "courier6", "courier7", "courier8"],
    
    lists:foreach(fun(CourierId) ->
        InitialInfo = #{
            id => list_to_binary(CourierId),
            status => <<"idle">>,
            current_package => null,
            delivered_packages => [],
            total_delivered => 0,
            zone => null,
            eta => null,
            last_update => erlang:system_time(second)
        },
        ets:insert(courier_states, {CourierId, InitialInfo})
    end, CourierIds).

%% בניית מידע על שליח - מתוקן לשמור נכון את total_delivered
build_courier_info(CourierId, NewState, ExistingInfo) ->
    %% מחלץ את הנתונים החדשים
    NewStatus = maps:get(status, NewState, idle),
    NewPackage = maps:get(package, NewState, null),
    NewZone = maps:get(zone, NewState, null),
    NewEta = maps:get(eta, NewState, null),
    NewTotalDelivered = maps:get(total_delivered, NewState, undefined),
    NewDeliveredPackages = maps:get(delivered_packages, NewState, undefined),
    
    %% מחלץ את הנתונים הקיימים
    ExistingTotalDelivered = maps:get(total_delivered, ExistingInfo, 0),
    ExistingDeliveredPackages = maps:get(delivered_packages, ExistingInfo, []),
    
    %% קביעת total_delivered הנכון
    FinalTotalDelivered = case NewTotalDelivered of
        undefined -> 
            %% אם לא הגיע מספר חדש, שמור את הקיים
            ExistingTotalDelivered;
        NewTotal -> 
            %% אם הגיע מספר חדש, השתמש בו
            NewTotal
    end,
    
    %% קביעת delivered_packages הנכון
    FinalDeliveredPackages = case NewDeliveredPackages of
        undefined -> 
            %% אם לא הגיעה רשימה חדשה, שמור את הקיימת
            ExistingDeliveredPackages;
        NewList -> 
            %% אם הגיעה רשימה חדשה, השתמש בה
            NewList
    end,
    
    %% פונקציה עזר להמרה ל-binary בצורה בטוחה
    ToBinary = fun(Val) ->
        case Val of
            Bin when is_binary(Bin) -> Bin;  %% כבר binary
            List when is_list(List) -> list_to_binary(List);  %% string
            Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);  %% atom
            Other -> list_to_binary(io_lib:format("~p", [Other]))  %% כל דבר אחר
        end
    end,
    
    %% בניית המידע המעודכן
    #{
        id => ToBinary(CourierId),
        status => ToBinary(NewStatus),
        current_package => case NewPackage of
            null -> null;
            Pkg -> ToBinary(Pkg)
        end,
        zone => case NewZone of
            null -> null;
            Zone -> ToBinary(Zone)
        end,
        eta => NewEta,
        delivered_packages => [ToBinary(P) || P <- FinalDeliveredPackages],
        total_delivered => FinalTotalDelivered,
        last_update => erlang:system_time(second)
    }.

%% בניית מידע על חבילה
build_package_info(PackageId, State) ->
    %% פונקציה עזר להמרה ל-binary בצורה בטוחה
    ToBinary = fun(Val) ->
        case Val of
            null -> null;
            Bin when is_binary(Bin) -> Bin;  %% כבר binary
            List when is_list(List) -> list_to_binary(List);  %% string
            Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);  %% atom
            Other -> list_to_binary(io_lib:format("~p", [Other]))  %% כל דבר אחר
        end
    end,
    
    #{
        id => ToBinary(PackageId),
        status => ToBinary(maps:get(status, State, ordered)),
        courier => ToBinary(maps:get(courier, State, null)),
        zone => ToBinary(maps:get(zone, State, null)),
        created_at => maps:get(created_at, State, erlang:system_time(second)),
        last_update => erlang:system_time(second)
    }.

%% בניית מידע על אזור
build_zone_info(Zone, State) ->
    %% פונקציה עזר להמרה ל-binary בצורה בטוחה
    ToBinary = fun(Val) ->
        case Val of
            Bin when is_binary(Bin) -> Bin;  %% כבר binary
            List when is_list(List) -> list_to_binary(List);  %% string
            Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);  %% atom
            Other -> list_to_binary(io_lib:format("~p", [Other]))  %% כל דבר אחר
        end
    end,
    
    #{
        zone => ToBinary(Zone),
        waiting_packages => maps:get(waiting_packages, State, 0),
        active_deliveries => maps:get(active_deliveries, State, 0),
        total_delivered => maps:get(total_delivered, State, 0),
        failed_deliveries => maps:get(failed_deliveries, State, 0),
        last_update => erlang:system_time(second)
    }.

%% שידור עדכון לכל המנויים
broadcast_update(UpdateType, Data, State) ->
    Subscribers = maps:get(subscribers, State),
    lists:foreach(fun(Subscriber) ->
        Subscriber ! {state_update, UpdateType, Data}
    end, Subscribers).

%% בדיקת מצב כל השליחים
check_all_couriers() ->
    CourierIds = ["courier1", "courier2", "courier3", "courier4", 
                  "courier5", "courier6", "courier7", "courier8"],
    
    lists:foreach(fun(CourierId) ->
        case whereis(list_to_atom("courier_" ++ CourierId)) of
            undefined ->
                %% השליח לא פעיל
                courier_state_changed(CourierId, #{status => offline});
            _Pid ->
                %% TODO: לקרוא למצב האמיתי של השליח
                ok
        end
    end, CourierIds).

%% בדיקת מצב כל האזורים  
check_all_zones() ->
    %% בדיקה שהטבלאות קיימות
    case ets:info(zone_states) of
        undefined -> ok;
        _ ->
            Zones = ["north", "center", "south"],
            
            lists:foreach(fun(Zone) ->
                case whereis(list_to_atom("zone_manager_" ++ Zone)) of
                    undefined ->
                        %% האזור לא פעיל
                        zone_state_changed(Zone, #{status => offline});
                    _Pid ->
                        %% TODO: לקרוא למצב האמיתי של האזור
                        ok
                end
            end, Zones)
    end.