%% -----------------------------------------------------------
%% מודול שרת המפה (Map Server)
%% gen_server שמנהל את מצב המפה ומספק API לגישה למידע
%% -- גרסה משודרגת עם יכולות ניתוב --
%% -----------------------------------------------------------
-module(map_server).
-behaviour(gen_server).

%% API
-export([start_link/0, initialize_map/0, initialize_map/1]).
-export([get_location/1, get_distance/2, get_zone_info/1]).
-export([update_courier_position/2, get_courier_position/1, get_all_courier_positions/0]).
-export([get_business_in_zone/1, get_random_home_in_zone/1]).
-export([get_route_distance/2, get_neighbors/1]).
-export([get_random_location/0]).
-export([get_route/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% כלול את קובץ ה-header עם הגדרות ה-records
-include("map_records.hrl").

%% רשומות פנימיות
-record(state, {
    initialized = false,     % האם המפה אותחלה
    num_homes = 200,        % מספר בתים
    courier_positions = #{} % מיקומי שליחים נוכחיים
}).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

initialize_map() ->
    gen_server:call(?MODULE, {initialize_map, 200}).

initialize_map(NumHomes) ->
    gen_server:call(?MODULE, {initialize_map, NumHomes}).

get_location(LocationId) ->
    gen_server:call(?MODULE, {get_location, LocationId}).

get_distance(FromId, ToId) ->
    gen_server:call(?MODULE, {get_distance, FromId, ToId}).

get_zone_info(Zone) ->
    gen_server:call(?MODULE, {get_zone_info, Zone}).

update_courier_position(CourierId, PositionData) ->
    gen_server:cast(?MODULE, {update_courier_position, CourierId, PositionData}).

get_courier_position(CourierId) ->
    gen_server:call(?MODULE, {get_courier_position, CourierId}).

get_all_courier_positions() ->
    gen_server:call(?MODULE, get_all_courier_positions).

get_business_in_zone(Zone) ->
    gen_server:call(?MODULE, {get_business_in_zone, Zone}).

get_random_home_in_zone(Zone) ->
    gen_server:call(?MODULE, {get_random_home_in_zone, Zone}).

get_random_location() ->
    gen_server:call(?MODULE, get_random_location).

get_route_distance(FromId, ToId) ->
    gen_server:call(?MODULE, {get_route_distance, FromId, ToId}).

get_neighbors(LocationId) ->
    gen_server:call(?MODULE, {get_neighbors, LocationId}).

get_route(FromId, ToId) ->
    gen_server:call(?MODULE, {get_route, FromId, ToId}).


%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Map Server starting...~n"),
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    {ok, #state{}}.

handle_call({initialize_map, NumHomes}, _From, State) ->
    io:format("Map Server: Initializing map with ~p homes...~n", [NumHomes]),
    case map_generator:generate_map(NumHomes) of
        {ok, MapData} ->
            report_map_initialized(MapData),
            {reply, {ok, map_initialized}, State#state{
                initialized = true,
                num_homes = NumHomes
            }};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_location, LocationId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            case ets:lookup(map_locations, LocationId) of
                [{_, Location}] ->
                    {reply, {ok, Location}, State};
                [] ->
                    {reply, {error, location_not_found}, State}
            end
    end;

handle_call({get_distance, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            Distance = calculate_direct_distance(FromId, ToId),
            {reply, Distance, State}
    end;

handle_call({get_zone_info, Zone}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            Info = get_zone_statistics(Zone),
            {reply, {ok, Info}, State}
    end;

handle_call({get_courier_position, CourierId}, _From, State) ->
    case maps:get(CourierId, State#state.courier_positions, undefined) of
        undefined ->
            {reply, {error, courier_not_found}, State};
        Position ->
            {reply, {ok, Position}, State}
    end;

handle_call(get_all_courier_positions, _From, State) ->
    {reply, {ok, State#state.courier_positions}, State};

handle_call({get_business_in_zone, Zone}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            BusinessId = "business_" ++ atom_to_list(Zone),
            case ets:lookup(map_locations, BusinessId) of
                [{_, Business}] ->
                    {reply, {ok, Business}, State};
                [] ->
                    {reply, {error, business_not_found}, State}
            end
    end;

handle_call({get_random_home_in_zone, Zone}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            AllLocations = ets:tab2list(map_locations),
            HomesInZone = [L || {_, L} <- AllLocations,
                                L#location.type == home,
                                L#location.zone == Zone],

            case HomesInZone of
                [] ->
                    {reply, {error, no_homes_in_zone}, State};
                Homes ->
                    RandomHome = lists:nth(rand:uniform(length(Homes)), Homes),
                    {reply, {ok, RandomHome}, State}
            end
    end;

handle_call(get_random_location, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            AllLocations = ets:tab2list(map_locations),
            case AllLocations of
                [] ->
                    {reply, {error, no_locations_on_map}, State};
                _ ->
                    {_, RandomLocation} = lists:nth(rand:uniform(length(AllLocations)), AllLocations),
                    {reply, {ok, RandomLocation}, State}
            end
    end;

handle_call({get_route_distance, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            case calculate_direct_distance(FromId, ToId) of
                {ok, DirectDistance} ->
                    RouteDistance = round(DirectDistance * 1.3),
                    {reply, {ok, RouteDistance}, State};
                Error ->
                    {reply, Error, State}
            end
    end;

handle_call({get_neighbors, LocationId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            case ets:lookup(map_graph, LocationId) of
                [{_, Neighbors}] ->
                    {reply, {ok, Neighbors}, State};
                [] ->
                    {reply, {error, location_not_found}, State}
            end
    end;

handle_call({get_route, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            case dijkstra(FromId, ToId) of
                {ok, Path} ->
                    io:format("~p: Found route from ~p to ~p: ~p~n", [?MODULE, FromId, ToId, Path]),
                    {reply, {ok, Path}, State};
                {error, Reason} ->
                    io:format("~p: Failed to find route from ~p to ~p: ~p~n", [?MODULE, FromId, ToId, Reason]),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_courier_position, CourierId, PositionData}, State) ->
    NewPositions = maps:put(CourierId, PositionData, State#state.courier_positions),
    report_courier_position_update(CourierId, PositionData),
    {noreply, State#state{courier_positions = NewPositions}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Map Server terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

calculate_direct_distance(FromId, ToId) ->
    case {ets:lookup(map_locations, FromId), ets:lookup(map_locations, ToId)} of
        {[{_, From}], [{_, To}]} ->
            DX = From#location.x - To#location.x,
            DY = From#location.y - To#location.y,
            Distance = round(math:sqrt(DX*DX + DY*DY)),
            {ok, Distance};
        _ ->
            {error, location_not_found}
    end.

get_zone_statistics(Zone) ->
    AllLocations = ets:tab2list(map_locations),

    LocationsInZone = [L || {_, L} <- AllLocations, L#location.zone == Zone],
    HomesInZone = [L || L <- LocationsInZone, L#location.type == home],
    BusinessesInZone = [L || L <- LocationsInZone, L#location.type == business],

    {CenterX, CenterY} = case LocationsInZone of
        [] -> {0, 0};
        Locs ->
            AvgX = lists:sum([L#location.x || L <- Locs]) div length(Locs),
            AvgY = lists:sum([L#location.y || L <- Locs]) div length(Locs),
            {AvgX, AvgY}
    end,

    #{
        zone => Zone,
        total_locations => length(LocationsInZone),
        homes => length(HomesInZone),
        businesses => length(BusinessesInZone),
        center => {CenterX, CenterY},
        home_ids => [H#location.id || H <- HomesInZone],
        business_ids => [B#location.id || B <- BusinessesInZone]
    }.

report_map_initialized(MapData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            ok;
        _ ->
            LocationsList = maps:get(locations, MapData, []),
            RoadsList = maps:get(roads, MapData, []),

            LocationsData = [location_to_map(L) || L <- LocationsList],
            RoadsData = [road_to_map(R) || R <- RoadsList],

            ZonesInfo = #{
                north => #{zone => <<"north">>},
                center => #{zone => <<"center">>},
                south => #{zone => <<"south">>}
            },

            Message = jsx:encode(#{
                type => <<"map_initialized">>,
                data => #{
                    locations => LocationsData,
                    roads => RoadsData,
                    zones => ZonesInfo
                }
            }),
            logistics_state_collector:broadcast_message(Message)
    end.

report_courier_position_update(CourierId, PositionData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            ok;
        _ ->
            SafePositionData = convert_position_data_to_safe(PositionData),
            Message = jsx:encode(#{
                type => <<"courier_position_update">>,
                data => maps:merge(#{courier_id => list_to_binary(CourierId)}, SafePositionData)
            }),
            logistics_state_collector:broadcast_message(Message)
    end.

convert_position_data_to_safe(PositionData) ->
    maps:fold(fun(Key, Value, Acc) ->
        SafeKey = convert_to_binary(Key),
        SafeValue = convert_value_to_safe(Value),
        maps:put(SafeKey, SafeValue, Acc)
    end, #{}, PositionData).

convert_value_to_safe(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
convert_value_to_safe(Value) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true -> list_to_binary(Value);
        false -> list_to_binary(io_lib:format("~p", [Value]))
    end;
convert_value_to_safe(Value) when is_map(Value) ->
    maps:fold(fun(K, V, Acc) ->
        maps:put(convert_to_binary(K), convert_value_to_safe(V), Acc)
    end, #{}, Value);
convert_value_to_safe(Value) when is_binary(Value) ->
    Value;
convert_value_to_safe(Value) when is_number(Value) ->
    Value;
convert_value_to_safe(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

convert_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
convert_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
convert_to_binary(Value) when is_binary(Value) ->
    Value;
convert_to_binary(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

location_to_map(Location) ->
    #{
        id => list_to_binary(Location#location.id),
        type => atom_to_binary(Location#location.type, utf8),
        zone => atom_to_binary(Location#location.zone, utf8),
        x => Location#location.x,
        y => Location#location.y,
        address => list_to_binary(Location#location.address)
    }.

road_to_map(Road) ->
    #{
        id => list_to_binary(Road#road.id),
        from => list_to_binary(Road#road.from),
        to => list_to_binary(Road#road.to),
        distance => Road#road.distance,
        base_time => Road#road.base_time
    }.

%% --- מימוש של אלגוריתם דייקסטרה ---

dijkstra(StartNode, EndNode) ->
    Nodes = [Id || {Id, _} <- ets:tab2list(map_locations)],
    Distances = maps:from_list([{Node, infinity} || Node <- Nodes]),
    DistancesWithStart = maps:put(StartNode, 0, Distances),
    PriorityQueue = gb_sets:from_list(Nodes),
    Previous = maps:new(),
    case dijkstra_loop(EndNode, PriorityQueue, DistancesWithStart, Previous) of
        {ok, FinalPrev} ->
            %% --- התיקון כאן: הסרת הקריאה המיותרת ל-lists:reverse/1 ---
            {ok, reconstruct_path(EndNode, FinalPrev, [])};
            %% --- סוף התיקון ---
        {error, Reason} ->
            {error, Reason}
    end.

dijkstra_loop(EndNode, PQ, Distances, Previous) ->
    case find_closest_node(PQ, Distances) of
        {U, _Dist} when U == EndNode ->
            {ok, Previous};
        {_U, infinity} ->
            {error, no_path_found};
        {U, DistU} ->
            NewPQ = gb_sets:delete(U, PQ),
            case ets:lookup(map_graph, U) of
                [{_, Neighbors}] ->
                    {NewDistances, NewPrevious} = lists:foldl(
                        fun({V, EdgeWeight, _}, {D, P}) ->
                            Alt = DistU + EdgeWeight,
                            CurrentDistV = maps:get(V, D),
                            if Alt < CurrentDistV ->
                                {maps:put(V, Alt, D), maps:put(V, U, P)};
                               true ->
                                {D, P}
                            end
                        end,
                        {Distances, Previous},
                        Neighbors
                    ),
                    dijkstra_loop(EndNode, NewPQ, NewDistances, NewPrevious);
                [] ->
                    dijkstra_loop(EndNode, NewPQ, Distances, Previous)
            end;
        none ->
            {error, no_path_found}
    end.

find_closest_node(PQ, Distances) ->
    gb_sets:fold(
        fun(Node, Closest) ->
            {_, ClosestDist} = Closest,
            Dist = maps:get(Node, Distances),
            if Dist < ClosestDist -> {Node, Dist};
               true -> Closest
            end
        end,
        {none, infinity},
        PQ
    ).

reconstruct_path(Current, Previous, Path) ->
    case maps:get(Current, Previous, undefined) of
        undefined ->
            [Current | Path];
        PrevNode ->
            reconstruct_path(PrevNode, Previous, [Current | Path])
    end.