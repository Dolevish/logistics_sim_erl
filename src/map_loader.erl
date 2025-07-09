%% -----------------------------------------------------------
%% מודול טוען המפה (Map Loader) - גרסה 11 (תיקון ניווט סופי)
%% מבטיח יצירת גרף דו-כיווני מלא ומסלולים אופטימליים.
%% -----------------------------------------------------------
-module(map_loader).
-export([load_map/0]).

-include("map_records.hrl").

%% @doc פונקציה ראשית לטעינת המפה.
load_map() ->
    io:format("Loading static map and DECONSTRUCTING roads from JSON...~n"),
    case load_and_parse_json_map() of
        {ok, {Locations, Roads, RawJsonForFrontend}} ->
            save_map_to_ets(Locations, Roads),
            print_map_statistics(Locations, Roads),
            {ok, RawJsonForFrontend};
        {error, Reason} ->
            io:format("Error loading map from JSON: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc פונקציית על: קוראת את קובץ ה-JSON ומעבדת אותו במלואו.
load_and_parse_json_map() ->
    try
        PrivDir = code:priv_dir(logistics_sim),
        FilePath = filename:join([PrivDir, "static", "map_data.json"]),
        {ok, BinaryData} = file:read_file(FilePath),
        JsonData = jsx:decode(BinaryData, [return_maps]),
        Elements = maps:get(<<"elements">>, JsonData),

        NamedLocations = parse_named_locations(Elements),
        
        JsonRoads = maps:get(<<"roads">>, Elements, []) ++ maps:get(<<"driveways">>, Elements, []),
        AllLocations = create_all_locations(JsonRoads, NamedLocations),

        LogicalRoads = deconstruct_and_rebuild_roads(JsonRoads, AllLocations),

        {ok, {AllLocations, LogicalRoads, BinaryData}}
    catch
        Type:Error:Stacktrace -> 
            io:format("Map loader crash: ~p:~p~n~p~n", [Type, Error, Stacktrace]),
            {error, {Type, Error}}
    end.

%% @doc טוען רק את המיקומים בעלי השם (בתים ועסקים).
parse_named_locations(Elements) ->
    HomesData = maps:get(<<"homes">>, Elements, []),
    BusinessesData = maps:get(<<"businesses">>, Elements, []),
    {HomesList, _} = lists:mapfoldl(
        fun(JsonLoc, Index) ->
            Id = "home_" ++ integer_to_list(Index),
            {#location{
                id = Id, type = home, zone = binary_to_atom(maps:get(<<"zone">>, JsonLoc), utf8),
                x = round(maps:get(<<"x">>, JsonLoc)), y = round(maps:get(<<"y">>, JsonLoc)),
                address = "Address for " ++ Id}, Index + 1}
        end, 1, HomesData),
    Businesses = lists:map(
        fun(JsonLoc) ->
            ZoneBin = maps:get(<<"zone">>, JsonLoc),
            Id = "business_" ++ binary_to_list(ZoneBin),
            #location{
                id = Id, type = business, zone = binary_to_atom(ZoneBin, utf8),
                x = round(maps:get(<<"x">>, JsonLoc)), y = round(maps:get(<<"y">>, JsonLoc)),
                address = "Business Center " ++ binary_to_list(ZoneBin)
            }
        end, BusinessesData),
    HomesList ++ Businesses.

%% @doc יוצר רשימה מלאה של כל המיקומים, כולל צמתים וירטואליים.
create_all_locations(JsonRoads, NamedLocations) ->
    NamedCoords = sets:from_list([ {L#location.x, L#location.y} || L <- NamedLocations ]),
    AllRoadEndpoints = lists:foldl(fun(Road, Acc) ->
        sets:add_element({round(maps:get(<<"x1">>, Road)), round(maps:get(<<"y1">>, Road))},
        sets:add_element({round(maps:get(<<"x2">>, Road)), round(maps:get(<<"y2">>, Road))}, Acc))
    end, sets:new(), JsonRoads),

    IntersectionCoords = find_intersections(JsonRoads),
    AllJunctions = sets:union(AllRoadEndpoints, IntersectionCoords),
    JunctionCoords = sets:subtract(AllJunctions, NamedCoords),
    {Junctions, _} = lists:mapfoldl(
        fun({X, Y}, Index) ->
            Id = "junction_" ++ integer_to_list(Index),
            {#location{id = Id, type = junction, zone = road, x = X, y = Y, address = "Junction"}, Index + 1}
        end, 1, sets:to_list(JunctionCoords)),
    NamedLocations ++ Junctions.

%% @doc הלוגיקה המרכזית: מפרקת ובונה מחדש את הכבישים.
deconstruct_and_rebuild_roads(JsonRoads, AllLocations) ->
    AllRoadSegments = lists:flatmap(
        fun(JsonRoad) ->
            X1 = round(maps:get(<<"x1">>, JsonRoad)), Y1 = round(maps:get(<<"y1">>, JsonRoad)),
            X2 = round(maps:get(<<"x2">>, JsonRoad)), Y2 = round(maps:get(<<"y2">>, JsonRoad)),
            
            PointsOnSegment = find_points_on_segment({X1,Y1}, {X2,Y2}, AllLocations),
            
            SortedPoints = lists:sort(
                fun(A, B) -> 
                    dist_sq({A#location.x, A#location.y}, {X1,Y1}) < dist_sq({B#location.x, B#location.y}, {X1,Y1})
                end,
                PointsOnSegment
            ),
            
            create_roads_from_sorted_list(SortedPoints)
        end,
        JsonRoads
    ),
    remove_duplicate_roads(AllRoadSegments).

%% @doc מוצא את כל המיקומים שנמצאים על קטע ישר בין שתי נקודות.
find_points_on_segment({X1,Y1}, {X2,Y2}, AllLocations) ->
    lists:filter(
        fun(Loc) ->
            PX = Loc#location.x, PY = Loc#location.y,
            IsOnLine = (min(X1,X2) - 1 =< PX andalso PX =< max(X1,X2) + 1) andalso
                       (min(Y1,Y2) - 1 =< PY andalso PY =< max(Y1,Y2) + 1),
            IsCollinear = (Y1-Y2)*(PX-X2) == (PY-Y2)*(X1-X2),
            IsOnLine andalso IsCollinear
        end,
        AllLocations
    ).

%% @doc יוצר רשומות #road מתוך רשימה ממוינת של מיקומים.
create_roads_from_sorted_list(SortedLocations) ->
    create_roads_recursive(SortedLocations, []).

create_roads_recursive([_], Acc) -> Acc;
create_roads_recursive([], Acc) -> Acc;
create_roads_recursive([L1, L2 | Rest], Acc) ->
    Dist = calculate_distance(L1, L2),
    Roads = [
        #road{id = L1#location.id ++ "_to_" ++ L2#location.id, from = L1#location.id, to = L2#location.id, distance = Dist, base_time = calculate_base_time(Dist)},
        #road{id = L2#location.id ++ "_to_" ++ L1#location.id, from = L2#location.id, to = L1#location.id, distance = Dist, base_time = calculate_base_time(Dist)}
    ],
    create_roads_recursive([L2 | Rest], Roads ++ Acc).


%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------
calculate_distance(L1, L2) ->
    DX = L1#location.x - L2#location.x, DY = L1#location.y - L2#location.y,
    round(math:sqrt(DX*DX + DY*DY)).

dist_sq({X1,Y1}, {X2,Y2}) ->
    DX = X1 - X2, DY = Y1 - Y2,
    DX*DX + DY*DY.

calculate_base_time(Distance) ->
    round(Distance / 11.11).

remove_duplicate_roads(Roads) ->
    RoadMap = lists:foldl(fun(Road, Map) ->
        Key = {Road#road.from, Road#road.to},
        maps:put(Key, Road, Map)
    end, #{}, Roads),
    maps:values(RoadMap).

%% @doc מחשב את כל נקודות החיתוך בין קטעי כביש ומחזיר אותן כ-Set.
find_intersections(JsonRoads) ->
    Roads = [ {round(maps:get(<<"x1">>, R)), round(maps:get(<<"y1">>, R)),
               round(maps:get(<<"x2">>, R)), round(maps:get(<<"y2">>, R))}
             || R <- JsonRoads ],
    find_intersections(Roads, [], sets:new()).

find_intersections([R | Rest], Processed, Acc) ->
    NewAcc = lists:foldl(fun(R2, A) ->
        case line_intersection(R, R2) of
            none -> A;
            {X, Y} -> sets:add_element({X, Y}, A)
        end
    end, Acc, Processed),
    find_intersections(Rest, [R | Processed], NewAcc);
find_intersections([], _Processed, Acc) ->
    Acc.

%% @doc בודק האם שני קטעים נחתכים ומחזיר את נקודת החיתוך העגולה.
line_intersection({X1,Y1,X2,Y2}, {X3,Y3,X4,Y4}) ->
    Den = (X1 - X2)*(Y3 - Y4) - (Y1 - Y2)*(X3 - X4),
    case Den of
        0 -> none; % מקבילים
        _ ->
            Px = ((X1*Y2 - Y1*X2)*(X3 - X4) - (X1 - X2)*(X3*Y4 - Y3*X4)) / Den,
            Py = ((X1*Y2 - Y1*X2)*(Y3 - Y4) - (Y1 - Y2)*(X3*Y4 - Y3*X4)) / Den,
            if
                Px >= min(X1,X2) andalso Px =< max(X1,X2) andalso
                Px >= min(X3,X4) andalso Px =< max(X3,X4) andalso
                Py >= min(Y1,Y2) andalso Py =< max(Y1,Y2) andalso
                Py >= min(Y3,Y4) andalso Py =< max(Y3,Y4) ->
                    {round(Px), round(Py)};
                true -> none
            end
    end.

%% -----------------------------------------------------------
%% שמירה ב-ETS ובניית הגרף
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
    Tables = [{map_locations, [named_table, public, {keypos, 1}]}, {map_roads, [named_table, public, {keypos, 1}]}, {map_graph, [named_table, public, {keypos, 1}]}],
    lists:foreach(fun({Name, Options}) -> case ets:info(Name) of undefined -> ets:new(Name, Options); _ -> ok end end, Tables).

build_graph_structure(Locations, Roads) ->
    RoadsByFrom = group_by(fun(R) -> R#road.from end, Roads),
    lists:foreach(fun(Loc) ->
        MyId = Loc#location.id,
        MyRoads = maps:get(MyId, RoadsByFrom, []),
        Neighbors = lists:map(fun(R) -> {R#road.to, R#road.distance, R#road.base_time} end, MyRoads),
        ets:insert(map_graph, {MyId, Neighbors})
    end, Locations).

group_by(Fun, List) ->
    lists:foldl(
        fun(Elem, Acc) ->
            Key = Fun(Elem),
            maps:put(Key, [Elem | maps:get(Key, Acc, [])], Acc)
        end,
        #{},
        List
    ).

%% -----------------------------------------------------------
%% הדפסת סטטיסטיקות
%% -----------------------------------------------------------
print_map_statistics(Locations, Roads) ->
    NumHomes = length([L || L <- Locations, L#location.type == home]),
    NumBusinesses = length([L || L <- Locations, L#location.type == business]),
    NumJunctions = length([L || L <- Locations, L#location.type == junction]),
    io:format("~n=== Static Map Loading Complete (FINAL LOGIC) ===~n"),
    io:format("Total Locations (Nodes): ~p~n", [length(Locations)]),
    io:format("  - Homes: ~p~n", [NumHomes]),
    io:format("  - Businesses: ~p~n", [NumBusinesses]),
    io:format("  - Road Junctions: ~p~n", [NumJunctions]),
    io:format("Total Directed Roads (Edges): ~p~n", [length(Roads)]),
    io:format("==================================================~n~n").