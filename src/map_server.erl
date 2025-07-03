%% -----------------------------------------------------------
%% מודול שרת המפה (Map Server)
%% gen_server שמנהל את מצב המפה ומספק API לגישה למידע
%% -----------------------------------------------------------
-module(map_server).
-behaviour(gen_server).

%% API
-export([start_link/0, initialize_map/0, initialize_map/1]).
-export([get_location/1, get_distance/2, get_zone_info/1]).
-export([update_courier_position/2, get_courier_position/1, get_all_courier_positions/0]).
-export([get_business_in_zone/1, get_random_home_in_zone/1]).
-export([get_route_distance/2, get_neighbors/1]).

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

%% התחלת השרת
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% אתחול המפה עם 200 בתים (ברירת מחדל)
initialize_map() ->
    gen_server:call(?MODULE, {initialize_map, 200}).

%% אתחול המפה עם מספר בתים מותאם
initialize_map(NumHomes) ->
    gen_server:call(?MODULE, {initialize_map, NumHomes}).

%% קבלת מידע על לוקיישן
get_location(LocationId) ->
    gen_server:call(?MODULE, {get_location, LocationId}).

%% קבלת מרחק בין שתי נקודות
get_distance(FromId, ToId) ->
    gen_server:call(?MODULE, {get_distance, FromId, ToId}).

%% קבלת מידע על אזור
get_zone_info(Zone) ->
    gen_server:call(?MODULE, {get_zone_info, Zone}).

%% עדכון מיקום שליח
update_courier_position(CourierId, PositionData) ->
    gen_server:cast(?MODULE, {update_courier_position, CourierId, PositionData}).

%% קבלת מיקום שליח
get_courier_position(CourierId) ->
    gen_server:call(?MODULE, {get_courier_position, CourierId}).

%% קבלת מיקומי כל השליחים
get_all_courier_positions() ->
    gen_server:call(?MODULE, get_all_courier_positions).

%% קבלת העסק באזור מסוים
get_business_in_zone(Zone) ->
    gen_server:call(?MODULE, {get_business_in_zone, Zone}).

%% קבלת בית רנדומלי באזור
get_random_home_in_zone(Zone) ->
    gen_server:call(?MODULE, {get_random_home_in_zone, Zone}).

%% קבלת מרחק לאורך מסלול
get_route_distance(FromId, ToId) ->
    gen_server:call(?MODULE, {get_route_distance, FromId, ToId}).

%% קבלת שכנים של נקודה
get_neighbors(LocationId) ->
    gen_server:call(?MODULE, {get_neighbors, LocationId}).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Map Server starting...~n"),
    
    %% אתחול מחולל מספרים רנדומליים
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    
    {ok, #state{}}.

%% אתחול המפה
handle_call({initialize_map, NumHomes}, _From, State) ->
    io:format("Map Server: Initializing map with ~p homes...~n", [NumHomes]),
    
    %% יצירת המפה
    case map_generator:generate_map(NumHomes) of
        {ok, MapData} ->
            %% דיווח לstate collector
            report_map_initialized(MapData),
            
            {reply, {ok, map_initialized}, State#state{
                initialized = true,
                num_homes = NumHomes
            }};
        Error ->
            {reply, Error, State}
    end;

%% קבלת מידע על לוקיישן
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

%% קבלת מרחק בין שתי נקודות
handle_call({get_distance, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            Distance = calculate_direct_distance(FromId, ToId),
            {reply, Distance, State}
    end;

%% קבלת מידע על אזור
handle_call({get_zone_info, Zone}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            Info = get_zone_statistics(Zone),
            {reply, {ok, Info}, State}
    end;

%% קבלת מיקום שליח
handle_call({get_courier_position, CourierId}, _From, State) ->
    case maps:get(CourierId, State#state.courier_positions, undefined) of
        undefined ->
            {reply, {error, courier_not_found}, State};
        Position ->
            {reply, {ok, Position}, State}
    end;

%% קבלת מיקומי כל השליחים
handle_call(get_all_courier_positions, _From, State) ->
    {reply, {ok, State#state.courier_positions}, State};

%% קבלת העסק באזור
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

%% קבלת בית רנדומלי באזור
handle_call({get_random_home_in_zone, Zone}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            %% מצא את כל הבתים באזור
            AllLocations = ets:tab2list(map_locations),
            HomesInZone = [L || {_, L} <- AllLocations, 
                                L#location.type == home,
                                L#location.zone == Zone],
            
            case HomesInZone of
                [] ->
                    {reply, {error, no_homes_in_zone}, State};
                Homes ->
                    %% בחר בית רנדומלי
                    RandomHome = lists:nth(rand:uniform(length(Homes)), Homes),
                    {reply, {ok, RandomHome}, State}
            end
    end;

%% קבלת מרחק לאורך מסלול
handle_call({get_route_distance, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            %% כרגע נחזיר מרחק ישיר * 1.3 (כדי לסמלץ נסיעה בכבישים)
            case calculate_direct_distance(FromId, ToId) of
                {ok, DirectDistance} ->
                    RouteDistance = round(DirectDistance * 1.3),
                    {reply, {ok, RouteDistance}, State};
                Error ->
                    {reply, Error, State}
            end
    end;

%% קבלת שכנים של נקודה
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

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% עדכון מיקום שליח
handle_cast({update_courier_position, CourierId, PositionData}, State) ->
    %% עדכון המיקום במפה הפנימית
    NewPositions = maps:put(CourierId, PositionData, State#state.courier_positions),
    
    %% דיווח לstate collector
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

%% חישוב מרחק ישיר בין שתי נקודות
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

%% קבלת סטטיסטיקות על אזור
get_zone_statistics(Zone) ->
    AllLocations = ets:tab2list(map_locations),
    
    %% ספירת בתים ועסקים באזור
    LocationsInZone = [L || {_, L} <- AllLocations, L#location.zone == Zone],
    HomesInZone = [L || L <- LocationsInZone, L#location.type == home],
    BusinessesInZone = [L || L <- LocationsInZone, L#location.type == business],
    
    %% חישוב מרכז האזור
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

%% דיווח על אתחול המפה
report_map_initialized(MapData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            ok;
        _ ->
            %% שליחת מידע על המפה שנוצרה
            LocationsList = maps:get(locations, MapData, []),
            RoadsList = maps:get(roads, MapData, []),
            
            %% הכנת המידע לשליחה
            LocationsData = [location_to_map(L) || L <- LocationsList],
            RoadsData = [road_to_map(R) || R <- RoadsList],
            
            Message = jsx:encode(#{
                type => <<"map_initialized">>,
                data => #{
                    locations => LocationsData,
                    roads => RoadsData,
                    zones => get_all_zones_info()
                }
            }),
            
            logistics_state_collector:broadcast_message(Message)
    end.

%% דיווח על עדכון מיקום שליח
report_courier_position_update(CourierId, PositionData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            ok;
        _ ->
            Message = jsx:encode(#{
                type => <<"courier_position_update">>,
                data => maps:merge(#{courier_id => list_to_binary(CourierId)}, PositionData)
            }),
            
            logistics_state_collector:broadcast_message(Message)
    end.

%% המרת location לmap
location_to_map(Location) ->
    #{
        id => list_to_binary(Location#location.id),
        type => atom_to_binary(Location#location.type, utf8),
        zone => atom_to_binary(Location#location.zone, utf8),
        x => Location#location.x,
        y => Location#location.y,
        address => list_to_binary(Location#location.address)
    }.

%% המרת road לmap
road_to_map(Road) ->
    #{
        id => list_to_binary(Road#road.id),
        from => list_to_binary(Road#road.from),
        to => list_to_binary(Road#road.to),
        distance => Road#road.distance,
        base_time => Road#road.base_time
    }.

%% קבלת מידע על כל האזורים
get_all_zones_info() ->
    #{
        north => get_zone_statistics(north),
        center => get_zone_statistics(center),
        south => get_zone_statistics(south)
    }.