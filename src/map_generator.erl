%% -----------------------------------------------------------
%% מודול יצירת המפה (Map Generator)
%% אחראי על יצירת המפה הראשונית עם מספר בתים דינמי ו-3 עסקים
%% יוצר רשת כבישים ריאלית ומחשב מרחקים
%% -- גרסה מתוקנת עם גרף קשיר ופריסה מרווחת --
%% -----------------------------------------------------------
-module(map_generator).
-export([generate_map/0, generate_map/1]).

%% כלול את קובץ ה-header עם הגדרות ה-records
-include("map_records.hrl").

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

generate_map() ->
    generate_map(200).

%% יצירת מפה עם מספר בתים מותאם אישית
generate_map(NumHomes) when is_integer(NumHomes), NumHomes >= 100, NumHomes =< 2000 ->
    io:format("Generating map with ~p homes and 3 businesses...~n", [NumHomes]),
    {Homes, Businesses} = generate_locations(NumHomes),
    AllLocations = Homes ++ Businesses,
    Roads = generate_road_network(AllLocations),
    save_map_to_ets(AllLocations, Roads),
    print_map_statistics(AllLocations, Roads),
    {ok, #{locations => AllLocations, roads => Roads}};
generate_map(NumHomes) ->
    {error, {invalid_number_of_homes, NumHomes}}.


%% -----------------------------------------------------------
%% פונקציות פרטיות - יצירת לוקיישנים
%% -----------------------------------------------------------

generate_locations(NumHomes) ->
    HomesPerZone = NumHomes div 3,
    ExtraHomes = NumHomes rem 3,

    %% --- הערה חדשה: היפוך קואורדינטות Y כדי להתאים ל-Canvas ---
    %% ב-Canvas, ציר Y גדל כלפי מטה. לכן, אזור הצפון צריך להיות עם ערכי Y נמוכים.
    NorthCenterY = 3000,
    CenterCenterY = 8000,
    SouthCenterY = 13000,
    CenterX = 8000,

    NorthHomes = generate_homes_for_zone(north, 1, HomesPerZone + ExtraHomes, CenterX, NorthCenterY),
    CenterHomes = generate_homes_for_zone(center, HomesPerZone + ExtraHomes + 1, HomesPerZone, CenterX, CenterCenterY),
    SouthHomes = generate_homes_for_zone(south, HomesPerZone * 2 + ExtraHomes + 1, HomesPerZone, CenterX, SouthCenterY),

    Businesses = [
        #location{
            id = "business_north", type = business, zone = north,
            x = CenterX, y = NorthCenterY - 500, % מעט מעל מרכז הבתים
            address = "North Business Center, Main St"
        },
        #location{
            id = "business_center", type = business, zone = center,
            x = CenterX, y = CenterCenterY,
            address = "Center Business Plaza, Commerce Ave"
        },
        #location{
            id = "business_south", type = business, zone = south,
            x = CenterX, y = SouthCenterY + 500, % מעט מתחת למרכז הבתים
            address = "South Business Park, Industrial Rd"
        }
    ],

    {NorthHomes ++ CenterHomes ++ SouthHomes, Businesses}.

generate_homes_for_zone(Zone, StartId, Count, CenterX, CenterY) ->
    lists:map(fun(I) ->
        Angle = (I / Count) * 2 * math:pi() + (rand:uniform() - 0.5),
        Radius = 1000 + rand:uniform(2500),
        X = round(CenterX + Radius * math:cos(Angle)),
        Y = round(CenterY + Radius * math:sin(Angle)),
        #location{
            id = "home_" ++ integer_to_list(StartId + I - 1),
            type = home, zone = Zone, x = X, y = Y,
            address = generate_address(Zone, StartId + I - 1)
        }
    end, lists:seq(1, Count)).

generate_address(Zone, HomeNum) ->
    Streets = ["Elm St", "Oak Ave", "Maple Rd", "Pine Blvd", "Cedar Dr"],
    Street = lists:nth((HomeNum rem length(Streets)) + 1, Streets),
    ZoneName = case Zone of north -> "North"; center -> "Center"; south -> "South" end,
    io_lib:format("~p ~s, ~s District", [HomeNum, Street, ZoneName]).

%% -----------------------------------------------------------
%% פונקציות פרטיות - יצירת רשת כבישים
%% -----------------------------------------------------------

generate_road_network(Locations) ->
    io:format("Generating road network...~n"),
    HomeToBusinessRoads = generate_home_to_business_roads(Locations),
    BusinessToBusinessRoads = generate_business_to_business_roads(Locations),
    HomeToHomeRoads = generate_nearby_home_roads(Locations),
    AllRoads = HomeToBusinessRoads ++ BusinessToBusinessRoads ++ HomeToHomeRoads,
    remove_duplicate_roads(AllRoads).

generate_home_to_business_roads(Locations) ->
    Homes = [L || L <- Locations, L#location.type == home],
    Businesses = [L || L <- Locations, L#location.type == business],
    lists:flatmap(fun(Home) ->
        BusinessInZone = lists:filter(fun(B) -> B#location.zone == Home#location.zone end, Businesses),
        case BusinessInZone of
            [Business] ->
                Distance = calculate_distance(Home, Business),
                [#road{id = Home#location.id ++ "_to_" ++ Business#location.id, from = Home#location.id, to = Business#location.id, distance = Distance, base_time = calculate_base_time(Distance)},
                 #road{id = Business#location.id ++ "_to_" ++ Home#location.id, from = Business#location.id, to = Home#location.id, distance = Distance, base_time = calculate_base_time(Distance)}];
            _ -> []
        end
    end, Homes).

generate_business_to_business_roads(Locations) ->
    Businesses = [L || L <- Locations, L#location.type == business],
    lists:flatmap(fun(I) ->
        B1 = lists:nth(I, Businesses),
        lists:flatmap(fun(J) ->
            if J > I ->
                B2 = lists:nth(J, Businesses),
                Distance = calculate_distance(B1, B2),
                [#road{id = B1#location.id ++ "_to_" ++ B2#location.id, from = B1#location.id, to = B2#location.id, distance = Distance, base_time = calculate_base_time(Distance)},
                 #road{id = B2#location.id ++ "_to_" ++ B1#location.id, from = B2#location.id, to = B1#location.id, distance = Distance, base_time = calculate_base_time(Distance)}];
            true -> []
            end
        end, lists:seq(1, length(Businesses)))
    end, lists:seq(1, length(Businesses))).

generate_nearby_home_roads(Locations) ->
    Homes = [L || L <- Locations, L#location.type == home],
    MaxDistance = 1500,
    lists:flatmap(fun(Home1) ->
        NearbyHomes = lists:filter(fun(Home2) -> Home1#location.id =/= Home2#location.id andalso calculate_distance(Home1, Home2) =< MaxDistance end, Homes),
        ConnectedHomes = lists:sublist(lists:sort(fun(H1, H2) -> calculate_distance(Home1, H1) =< calculate_distance(Home1, H2) end, NearbyHomes), 3),
        lists:flatmap(fun(Home2) ->
            Distance = calculate_distance(Home1, Home2),
            [#road{id = Home1#location.id ++ "_to_" ++ Home2#location.id, from = Home1#location.id, to = Home2#location.id, distance = Distance, base_time = calculate_base_time(Distance)},
             #road{id = Home2#location.id ++ "_to_" ++ Home1#location.id, from = Home2#location.id, to = Home1#location.id, distance = Distance, base_time = calculate_base_time(Distance)}]
        end, ConnectedHomes)
    end, Homes).

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------

calculate_distance(Loc1, Loc2) ->
    DX = Loc1#location.x - Loc2#location.x,
    DY = Loc1#location.y - Loc2#location.y,
    round(math:sqrt(DX*DX + DY*DY)).

calculate_base_time(Distance) ->
    round(Distance / 11.11).

remove_duplicate_roads(Roads) ->
    RoadMap = lists:foldl(fun(Road, Map) ->
        Key = {min(Road#road.from, Road#road.to), max(Road#road.from, Road#road.to)},
        maps:put(Key, Road, Map)
    end, #{}, Roads),
    maps:values(RoadMap).

%% -----------------------------------------------------------
%% שמירה ב-ETS
%% -----------------------------------------------------------

save_map_to_ets(Locations, Roads) ->
    io:format("Saving map data to ETS tables...~n"),
    ensure_ets_tables(),
    ets:delete_all_objects(map_locations),
    ets:delete_all_objects(map_roads),
    ets:delete_all_objects(map_graph),
    lists:foreach(fun(Loc) -> ets:insert(map_locations, {Loc#location.id, Loc}) end, Locations),
    lists:foreach(fun(Road) -> ets:insert(map_roads, {Road#road.id, Road}) end, Roads),
    build_graph_structure(Locations, Roads),
    io:format("Map data saved successfully~n").

ensure_ets_tables() ->
    Tables = [{map_locations, [named_table, public, {keypos, 1}]}, {map_roads, [named_table, public, {keypos, 1}]}, {map_graph, [named_table, public, {keypos, 1}]}],
    lists:foreach(fun({Name, Options}) -> case ets:info(Name) of undefined -> ets:new(Name, Options); _ -> ok end end, Tables).

build_graph_structure(Locations, Roads) ->
    lists:foreach(fun(Loc) ->
        MyId = Loc#location.id,
        RelatedRoads = [R || R <- Roads, R#road.from == MyId orelse R#road.to == MyId],
        Neighbors = lists:map(fun(Road) ->
            NeighborId = if Road#road.from == MyId -> Road#road.to; true -> Road#road.from end,
            {NeighborId, Road#road.distance, Road#road.base_time}
        end, RelatedRoads),
        ets:insert(map_graph, {MyId, Neighbors})
    end, Locations).

%% -----------------------------------------------------------
%% הדפסת סטטיסטיקות
%% -----------------------------------------------------------

print_map_statistics(Locations, Roads) ->
    NumHomes = length([L || L <- Locations, L#location.type == home]),
    NumBusinesses = length([L || L <- Locations, L#location.type == business]),
    NorthHomes = length([L || L <- Locations, L#location.type == home, L#location.zone == north]),
    CenterHomes = length([L || L <- Locations, L#location.type == home, L#location.zone == center]),
    SouthHomes = length([L || L <- Locations, L#location.type == home, L#location.zone == south]),
    io:format("~n=== Map Generation Complete ===~n"),
    io:format("Total Locations: ~p~n", [length(Locations)]),
    io:format("  - Homes: ~p~n", [NumHomes]),
    io:format("    * North: ~p~n", [NorthHomes]),
    io:format("    * Center: ~p~n", [CenterHomes]),
    io:format("    * South: ~p~n", [SouthHomes]),
    io:format("  - Businesses: ~p~n", [NumBusinesses]),
    io:format("Total Roads: ~p~n", [length(Roads)]),
    io:format("==============================~n~n").
