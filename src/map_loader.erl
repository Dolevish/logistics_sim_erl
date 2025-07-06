%% -----------------------------------------------------------
%% מודול טוען המפה (Map Loader)
%% אחראי על טעינת מפה סטטית מקובץ JSON
%% ויצירת גרף קשיר לניווט.
%% -----------------------------------------------------------
-module(map_loader).
-export([load_map/0]).

-include("map_records.hrl").

%% @doc פונקציה ראשית לטעינת המפה.
load_map() ->
    io:format("Loading static map from JSON file...~n"),
    case load_locations_from_json() of
        {ok, {Locations, RawJsonForFrontend}} ->
            Roads = generate_roads_from_locations(Locations),
            save_map_to_ets(Locations, Roads),
            print_map_statistics(Locations, Roads),
            {ok, RawJsonForFrontend};
        {error, Reason} ->
            io:format("Error loading map from JSON: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc טוען את המיקומים מקובץ ה-JSON.
load_locations_from_json() ->
    try
        PrivDir = code:priv_dir(logistics_sim),
        FilePath = filename:join([PrivDir, "static", "map_data.json"]),
        case file:read_file(FilePath) of
            {ok, BinaryData} ->
                JsonData = jsx:decode(BinaryData, [return_maps]),
                HomesData = maps:get(<<"homes">>, maps:get(<<"elements">>, JsonData)),
                BusinessesData = maps:get(<<"businesses">>, maps:get(<<"elements">>, JsonData)),

                Homes = parse_locations(HomesData, home, "home_"),
                Businesses = parse_businesses(BusinessesData),

                {ok, {Homes ++ Businesses, BinaryData}};
            {error, Reason} ->
                throw({error, {file_read_error, Reason}})
        end
    catch
        _:E -> {error, E}
    end.

%% @doc ממיר רשימת מיקומים מ-JSON לרשומות #location.
parse_locations(JsonLocations, Type, IdPrefix) ->
    {Locations, _} = lists:mapfoldl(
        fun(JsonLoc, Index) ->
            X = maps:get(<<"x">>, JsonLoc),
            Y = maps:get(<<"y">>, JsonLoc),
            Zone = maps:get(<<"zone">>, JsonLoc),
            Location = #location{
                id = IdPrefix ++ integer_to_list(Index),
                type = Type,
                zone = binary_to_atom(Zone, utf8),
                x = round(X),
                y = round(Y),
                address = "Address for " ++ IdPrefix ++ integer_to_list(Index)
            },
            {Location, Index + 1}
        end,
        1,
        JsonLocations
    ),
    Locations.

%% @doc ממיר את העסקים מ-JSON לרשומות #location עם ID לפי אזור.
parse_businesses(JsonBusinesses) ->
    lists:map(
        fun(JsonLoc) ->
            X = maps:get(<<"x">>, JsonLoc),
            Y = maps:get(<<"y">>, JsonLoc),
            Zone = maps:get(<<"zone">>, JsonLoc),
            Type = business,
            Id = "business_" ++ binary_to_list(Zone),
            #location{
                id = Id,
                type = Type,
                zone = binary_to_atom(Zone, utf8),
                x = round(X),
                y = round(Y),
                address = "Business Center " ++ binary_to_list(Zone)
            }
        end,
        JsonBusinesses
    ).


%% -----------------------------------------------------------
%% יצירת רשת כבישים (מבוסס על הלוגיקה המקורית)
%% -----------------------------------------------------------
generate_roads_from_locations(Locations) ->
    io:format("Generating road network based on loaded locations...~n"),
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
    MaxDistance = 1500, % פרמטר לקביעת קרבה, ניתן לכוונון
    lists:flatmap(fun(Home1) ->
        NearbyHomes = lists:filter(fun(Home2) ->
            Home1#location.id =/= Home2#location.id andalso
            Home1#location.zone == Home2#location.zone andalso % חבר רק בתים באותו אזור
            calculate_distance(Home1, Home2) =< MaxDistance
        end, Homes),
        % חבר לעד 3 השכנים הקרובים ביותר
        ConnectedHomes = lists:sublist(
            lists:sort(fun(H1, H2) -> calculate_distance(Home1, H1) =< calculate_distance(Home1, H2) end, NearbyHomes),
            3
        ),
        lists:flatmap(fun(Home2) ->
            Distance = calculate_distance(Home1, Home2),
            [#road{id = Home1#location.id ++ "_to_" ++ Home2#location.id, from = Home1#location.id, to = Home2#location.id, distance = Distance, base_time = calculate_base_time(Distance)},
             #road{id = Home2#location.id ++ "_to_" ++ Home1#location.id, from = Home2#location.id, to = Home1#location.id, distance = Distance, base_time = calculate_base_time(Distance)}]
        end, ConnectedHomes)
    end, Homes).

calculate_distance(Loc1, Loc2) ->
    DX = Loc1#location.x - Loc2#location.x,
    DY = Loc1#location.y - Loc2#location.y,
    round(math:sqrt(DX*DX + DY*DY)).

calculate_base_time(Distance) ->
    round(Distance / 11.11). % ~40 קמ"ש

remove_duplicate_roads(Roads) ->
    RoadMap = lists:foldl(fun(Road, Map) ->
        Key = {min(Road#road.from, Road#road.to), max(Road#road.from, Road#road.to)},
        maps:put(Key, Road, Map)
    end, #{}, Roads),
    maps:values(RoadMap).

%% -----------------------------------------------------------
%% שמירה ב-ETS ובניית הגרף (זהה ל-map_generator)
%% -----------------------------------------------------------
save_map_to_ets(Locations, Roads) ->
    io:format("Saving static map data to ETS tables...~n"),
    ensure_ets_tables(),
    ets:delete_all_objects(map_locations),
    ets:delete_all_objects(map_roads),
    ets:delete_all_objects(map_graph),
    lists:foreach(fun(Loc) -> ets:insert(map_locations, {Loc#location.id, Loc}) end, Locations),
    lists:foreach(fun(Road) -> ets:insert(map_roads, {Road#road.id, Road}) end, Roads),
    build_graph_structure(Locations, Roads),
    io:format("Map data saved successfully~n").

ensure_ets_tables() ->
    Tables = [
        {map_locations, [named_table, public, {keypos, 1}]},
        {map_roads, [named_table, public, {keypos, 1}]},
        {map_graph, [named_table, public, {keypos, 1}]}
    ],
    lists:foreach(fun({Name, Options}) ->
        case ets:info(Name) of
            undefined -> ets:new(Name, Options);
            _ -> ok
        end
    end, Tables).

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
%% הדפסת סטטיסטיקות (זהה ל-map_generator)
%% -----------------------------------------------------------
print_map_statistics(Locations, Roads) ->
    NumHomes = length([L || L <- Locations, L#location.type == home]),
    NumBusinesses = length([L || L <- Locations, L#location.type == business]),
    NorthHomes = length([L || L <- Locations, L#location.type == home, L#location.zone == north]),
    CenterHomes = length([L || L <- Locations, L#location.type == home, L#location.zone == center]),
    SouthHomes = length([L || L <- Locations, L#location.type == home, L#location.zone == south]),
    io:format("~n=== Static Map Loading Complete ===~n"),
    io:format("Total Locations: ~p~n", [length(Locations)]),
    io:format("  - Homes: ~p~n", [NumHomes]),
    io:format("    * North: ~p~n", [NorthHomes]),
    io:format("    * Center: ~p~n", [CenterHomes]),
    io:format("    * South: ~p~n", [SouthHomes]),
    io:format("  - Businesses: ~p~n", [NumBusinesses]),
    io:format("Total Roads in Graph: ~p~n", [length(Roads)]),
    io:format("===================================~n~n").