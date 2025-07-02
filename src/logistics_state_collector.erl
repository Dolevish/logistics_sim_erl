%% -----------------------------------------------------------
%% מודול איסוף מצב המערכת (State Collector) - מתוקן
%% אוסף מידע מכל הרכיבים במערכת ומפיץ עדכונים ל-WebSocket handlers
%% משמש כ-Event Bus מרכזי וכמקור אמת יחיד (Single Source of Truth) לממשק המשתמש
%% -----------------------------------------------------------
-module(logistics_state_collector).
-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, unsubscribe/1]).
-export([courier_state_changed/2, package_state_changed/2, zone_state_changed/2]).
-export([get_courier_info/1, get_package_info/1, get_zone_info/1, get_full_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe(HandlerPid) ->
    gen_server:cast(?MODULE, {subscribe, HandlerPid}).

unsubscribe(HandlerPid) ->
    gen_server:cast(?MODULE, {unsubscribe, HandlerPid}).

courier_state_changed(CourierId, NewState) ->
    gen_server:cast(?MODULE, {courier_update, CourierId, NewState}).

package_state_changed(PackageId, NewState) ->
    gen_server:cast(?MODULE, {package_update, PackageId, NewState}).

zone_state_changed(Zone, NewState) ->
    gen_server:cast(?MODULE, {zone_update, Zone, NewState}).

get_courier_info(CourierId) ->
    gen_server:call(?MODULE, {get_courier_info, CourierId}).

get_package_info(PackageId) ->
    gen_server:call(?MODULE, {get_package_info, PackageId}).

get_zone_info(Zone) ->
    gen_server:call(?MODULE, {get_zone_info, Zone}).

get_full_state() ->
    gen_server:call(?MODULE, get_full_state).


%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Logistics State Collector starting...~n"),
    
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
    
    init_courier_states(),
    
    erlang:send_after(5000, self(), check_system_state),
    
    {ok, #{
        subscribers => [],
        update_counter => 0
    }}.

%% טיפול ב-subscribe
handle_cast({subscribe, HandlerPid}, State) ->
    io:format("State Collector: New subscriber ~p~n", [HandlerPid]),
    Subscribers = maps:get(subscribers, State),
    erlang:monitor(process, HandlerPid),
    NewState = State#{subscribers => [HandlerPid | Subscribers]},
    {noreply, NewState};

%% טיפול ב-unsubscribe
handle_cast({unsubscribe, HandlerPid}, State) ->
    io:format("State Collector: Unsubscribing ~p~n", [HandlerPid]),
    Subscribers = maps:get(subscribers, State),
    NewState = State#{subscribers => lists:delete(HandlerPid, Subscribers)},
    {noreply, NewState};

%% טיפול בעדכון מצב שליח
handle_cast({courier_update, CourierId, NewState}, State) ->
    io:format("State Collector: Courier ~p state changed to ~p~n", [CourierId, NewState]),
    ExistingInfo = case ets:lookup(courier_states, CourierId) of
        [{_, Info}] -> Info;
        [] -> #{}
    end,
    UpdatedInfo = build_courier_info(CourierId, NewState, ExistingInfo),
    ets:insert(courier_states, {CourierId, UpdatedInfo}),
    broadcast_update(<<"courier_update">>, UpdatedInfo, State),
    Counter = maps:get(update_counter, State),
    {noreply, State#{update_counter => Counter + 1}};

%% טיפול בעדכון מצב חבילה
handle_cast({package_update, PackageId, NewState}, State) ->
    io:format("State Collector: Package ~p state changed to ~p~n", [PackageId, NewState]),
    PackageInfo = build_package_info(PackageId, NewState),
    ets:insert(package_states, {PackageId, PackageInfo}),
    broadcast_update(<<"package_update">>, PackageInfo, State),
    Counter = maps:get(update_counter, State),
    {noreply, State#{update_counter => Counter + 1}};

%% טיפול בעדכון מצב אזור
handle_cast({zone_update, Zone, NewState}, State) ->
    io:format("State Collector: Zone ~p state changed~n", [Zone]),
    case ets:info(zone_states) of
        undefined ->
            io:format("Warning: zone_states table not ready yet~n"),
            {noreply, State};
        _ ->
            ZoneInfo = build_zone_info(Zone, NewState),
            ets:insert(zone_states, {Zone, ZoneInfo}),
            broadcast_update(<<"zone_update">>, ZoneInfo, State),
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

handle_call(get_full_state, _From, State) ->
    Couriers = ets:tab2list(courier_states),
    Packages = ets:tab2list(package_states),
    Zones = ets:tab2list(zone_states),
    FullState = #{
        couriers => [V || {_, V} <- Couriers],
        packages => [V || {_, V} <- Packages],
        zones => [V || {_, V} <- Zones]
    },
    {reply, {ok, FullState}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% בדיקה תקופתית של מצב המערכת
handle_info(check_system_state, State) ->
    io:format("State Collector: Performing periodic system check~n"),
    check_all_couriers(),
    check_all_zones(),
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

build_courier_info(CourierId, NewState, ExistingInfo) ->
    NewStatus = maps:get(status, NewState, idle),
    NewPackage = maps:get(package, NewState, null),
    NewZone = maps:get(zone, NewState, null),
    NewEta = maps:get(eta, NewState, null),
    NewTotalDelivered = maps:get(total_delivered, NewState, undefined),
    NewDeliveredPackages = maps:get(delivered_packages, NewState, undefined),
    ExistingTotalDelivered = maps:get(total_delivered, ExistingInfo, 0),
    ExistingDeliveredPackages = maps:get(delivered_packages, ExistingInfo, []),
    FinalTotalDelivered = case NewTotalDelivered of
        undefined -> ExistingTotalDelivered;
        NewTotal -> NewTotal
    end,
    FinalDeliveredPackages = case NewDeliveredPackages of
        undefined -> ExistingDeliveredPackages;
        NewList -> NewList
    end,
    ToBinary = fun(Val) ->
        case Val of
            Bin when is_binary(Bin) -> Bin;
            List when is_list(List) -> list_to_binary(List);
            Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);
            Other -> list_to_binary(io_lib:format("~p", [Other]))
        end
    end,
    #{
        id => ToBinary(CourierId),
        status => ToBinary(NewStatus),
        current_package => case NewPackage of null -> null; Pkg -> ToBinary(Pkg) end,
        zone => case NewZone of null -> null; Zone -> ToBinary(Zone) end,
        eta => NewEta,
        delivered_packages => [ToBinary(P) || P <- FinalDeliveredPackages],
        total_delivered => FinalTotalDelivered,
        last_update => erlang:system_time(second)
    }.

build_package_info(PackageId, State) ->
    ToBinary = fun(Val) ->
        case Val of
            null -> null;
            Bin when is_binary(Bin) -> Bin;
            List when is_list(List) -> list_to_binary(List);
            Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);
            Other -> list_to_binary(io_lib:format("~p", [Other]))
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

%% הערה חדשה: הוספת השדה total_orders למידע על האזור
build_zone_info(Zone, State) ->
    ToBinary = fun(Val) ->
        case Val of
            Bin when is_binary(Bin) -> Bin;
            List when is_list(List) -> list_to_binary(List);
            Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);
            Other -> list_to_binary(io_lib:format("~p", [Other]))
        end
    end,
    #{
        zone => ToBinary(Zone),
        waiting_packages => maps:get(waiting_packages, State, 0),
        active_deliveries => maps:get(active_deliveries, State, 0),
        total_delivered => maps:get(total_delivered, State, 0),
        failed_deliveries => maps:get(failed_deliveries, State, 0),
        total_orders => maps:get(total_orders, State, 0), %% הוספת השדה
        last_update => erlang:system_time(second)
    }.

broadcast_update(UpdateType, Data, State) ->
    Subscribers = maps:get(subscribers, State),
    lists:foreach(fun(Subscriber) ->
        Subscriber ! {state_update, UpdateType, Data}
    end, Subscribers).

check_all_couriers() ->
    CourierIds = ["courier1", "courier2", "courier3", "courier4", 
                  "courier5", "courier6", "courier7", "courier8"],
    lists:foreach(fun(CourierId) ->
        case whereis(list_to_atom("courier_" ++ CourierId)) of
            undefined -> courier_state_changed(CourierId, #{status => offline});
            _Pid -> ok
        end
    end, CourierIds).

check_all_zones() ->
    case ets:info(zone_states) of
        undefined -> ok;
        _ ->
            Zones = ["north", "center", "south"],
            lists:foreach(fun(Zone) ->
                case whereis(list_to_atom("zone_manager_" ++ Zone)) of
                    undefined -> zone_state_changed(Zone, #{status => offline});
                    _Pid -> ok
                end
            end, Zones)
    end.
