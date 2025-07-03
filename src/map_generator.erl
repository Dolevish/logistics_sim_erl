%% -----------------------------------------------------------
%% מודול יצירת המפה (Map Generator)
%% אחראי על יצירת המפה הראשונית עם 200 בתים ו-3 עסקים
%% יוצר רשת כבישים ריאלית ומחשב מרחקים
%% -- גרסה מתוקנת עם גרף קשיר --
%% -----------------------------------------------------------
-module(map_generator).
-export([generate_map/0, generate_map/1]).

%% כלול את קובץ ה-header עם הגדרות ה-records
-include("map_records.hrl").

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

%% יצירת מפה עם 200 בתים (ברירת מחדל)
generate_map() ->
    generate_map(200).

%% יצירת מפה עם מספר בתים מותאם אישית
generate_map(NumHomes) ->
    io:format("Generating map with ~p homes and 3 businesses...~n", [NumHomes]),

    %% יצירת הלוקיישנים (בתים ועסקים)
    {Homes, Businesses} = generate_locations(NumHomes),
    AllLocations = Homes ++ Businesses,

    %% יצירת רשת הכבישים
    Roads = generate_road_network(AllLocations),

    %% שמירת המפה ב-ETS
    save_map_to_ets(AllLocations, Roads),

    %% הדפסת סטטיסטיקות
    print_map_statistics(AllLocations, Roads),

    {ok, #{locations => AllLocations, roads => Roads}}.

%% -----------------------------------------------------------
%% פונקציות פרטיות - יצירת לוקיישנים
%% -----------------------------------------------------------

%% יצירת בתים ועסקים
generate_locations(NumHomes) ->
    %% חלוקת הבתים לאזורים
    HomesPerZone = NumHomes div 3,
    ExtraHomes = NumHomes rem 3,

    %% יצירת בתים לכל אזור
    NorthHomes = generate_homes_for_zone(north, 1, HomesPerZone + ExtraHomes, 5000, 8000),
    CenterHomes = generate_homes_for_zone(center, HomesPerZone + ExtraHomes + 1, HomesPerZone, 5000, 5000),
    SouthHomes = generate_homes_for_zone(south, HomesPerZone * 2 + ExtraHomes + 1, HomesPerZone, 5000, 2000),

    %% יצירת עסקים - אחד במרכז כל אזור
    Businesses = [
        #location{
            id = "business_north",
            type = business,
            zone = north,
            x = 5000,
            y = 8500,
            address = "North Business Center, Main St"
        },
        #location{
            id = "business_center",
            type = business,
            zone = center,
            x = 5000,
            y = 5000,
            address = "Center Business Plaza, Commerce Ave"
        },
        #location{
            id = "business_south",
            type = business,
            zone = south,
            x = 5000,
            y = 1500,
            address = "South Business Park, Industrial Rd"
        }
    ],

    {NorthHomes ++ CenterHomes ++ SouthHomes, Businesses}.

%% יצירת בתים לאזור ספציפי
generate_homes_for_zone(Zone, StartId, Count, CenterX, CenterY) ->
    %% יצירת בתים בפיזור רנדומלי סביב נקודת מרכז
    lists:map(fun(I) ->
        %% חישוב מיקום רנדומלי בתוך רדיוס של 2000 מטר מהמרכז
        Angle = (I / Count) * 2 * math:pi(),
        Radius = 500 + rand:uniform(1500),
        X = round(CenterX + Radius * math:cos(Angle)),
        Y = round(CenterY + Radius * math:sin(Angle)),

        #location{
            id = "home_" ++ integer_to_list(StartId + I - 1),
            type = home,
            zone = Zone,
            x = X,
            y = Y,
            address = generate_address(Zone, StartId + I - 1)
        }
    end, lists:seq(1, Count)).

%% יצירת כתובת פיקטיבית
generate_address(Zone, HomeNum) ->
    Streets = ["Elm St", "Oak Ave", "Maple Rd", "Pine Blvd", "Cedar Dr"],
    Street = lists:nth((HomeNum rem length(Streets)) + 1, Streets),
    ZoneName = case Zone of
        north -> "North";
        center -> "Center";
        south -> "South"
    end,
    io_lib:format("~p ~s, ~s District", [HomeNum, Street, ZoneName]).

%% -----------------------------------------------------------
%% פונקציות פרטיות - יצירת רשת כבישים
%% -----------------------------------------------------------

%% --- תיקון: הבטחת גרף קשיר ---
%% @doc יצירת רשת כבישים המבטיחה שכל הנקודות מקושרות.
generate_road_network(Locations) ->
    io:format("Generating road network...~n"),

    %% שלב 1: חיבור כל בית לעסק המרכזי באזור שלו.
    %% זהו השלב הקריטי שמבטיח שאין "איים" מבודדים.
    HomeToBusinessRoads = generate_home_to_business_roads(Locations),

    %% שלב 2: יצירת כבישים ראשיים בין העסקים כדי לקשר בין האזורים.
    BusinessToBusinessRoads = generate_business_to_business_roads(Locations),

    %% שלב 3 (אופציונלי אך מומלץ): הוספת כבישים משניים בין בתים קרובים.
    %% זה יוצר רשת ריאליסטית יותר עם מסלולים אלטרנטיביים.
    HomeToHomeRoads = generate_nearby_home_roads(Locations),

    %% איחוד כל הכבישים והסרת כפילויות.
    AllRoads = HomeToBusinessRoads ++ BusinessToBusinessRoads ++ HomeToHomeRoads,
    remove_duplicate_roads(AllRoads).
%% --- סוף התיקון ---

%% @doc יצירת כבישים דו-כיווניים מכל בית לעסק באותו אזור.
generate_home_to_business_roads(Locations) ->
    Homes = [L || L <- Locations, L#location.type == home],
    Businesses = [L || L <- Locations, L#location.type == business],

    lists:flatmap(fun(Home) ->
        BusinessInZone = lists:filter(fun(B) -> B#location.zone == Home#location.zone end, Businesses),

        case BusinessInZone of
            [Business] ->
                Distance = calculate_distance(Home, Business),
                [
                    #road{
                        id = Home#location.id ++ "_to_" ++ Business#location.id,
                        from = Home#location.id,
                        to = Business#location.id,
                        distance = Distance,
                        base_time = calculate_base_time(Distance)
                    },
                    %% הבטחת כביש דו-כיווני
                    #road{
                        id = Business#location.id ++ "_to_" ++ Home#location.id,
                        from = Business#location.id,
                        to = Home#location.id,
                        distance = Distance,
                        base_time = calculate_base_time(Distance)
                    }
                ];
            _ -> []
        end
    end, Homes).

%% יצירת כבישים בין העסקים
generate_business_to_business_roads(Locations) ->
    Businesses = [L || L <- Locations, L#location.type == business],

    lists:flatmap(fun(I) ->
        B1 = lists:nth(I, Businesses),
        lists:flatmap(fun(J) ->
            if J > I ->
                B2 = lists:nth(J, Businesses),
                Distance = calculate_distance(B1, B2),
                [
                    #road{
                        id = B1#location.id ++ "_to_" ++ B2#location.id,
                        from = B1#location.id,
                        to = B2#location.id,
                        distance = Distance,
                        base_time = calculate_base_time(Distance)
                    },
                    #road{
                        id = B2#location.id ++ "_to_" ++ B1#location.id,
                        from = B2#location.id,
                        to = B1#location.id,
                        distance = Distance,
                        base_time = calculate_base_time(Distance)
                    }
                ];
            true -> []
            end
        end, lists:seq(1, length(Businesses)))
    end, lists:seq(1, length(Businesses))).

%% יצירת כבישים בין בתים קרובים
generate_nearby_home_roads(Locations) ->
    Homes = [L || L <- Locations, L#location.type == home],
    MaxDistance = 1000, % מרחק מקסימלי לחיבור בין בתים (במטרים)

    lists:flatmap(fun(Home1) ->
        NearbyHomes = lists:filter(fun(Home2) ->
            Home1#location.id =/= Home2#location.id andalso
            calculate_distance(Home1, Home2) =< MaxDistance
        end, Homes),

        %% צור כבישים דו-כיווניים לעד 3 הבתים הקרובים ביותר
        ConnectedHomes = lists:sublist(lists:sort(fun(H1, H2) ->
            calculate_distance(Home1, H1) =< calculate_distance(Home1, H2)
        end, NearbyHomes), 3),

        lists:flatmap(fun(Home2) ->
            Distance = calculate_distance(Home1, Home2),
            [
                #road{
                    id = Home1#location.id ++ "_to_" ++ Home2#location.id,
                    from = Home1#location.id,
                    to = Home2#location.id,
                    distance = Distance,
                    base_time = calculate_base_time(Distance)
                },
                #road{
                    id = Home2#location.id ++ "_to_" ++ Home1#location.id,
                    from = Home2#location.id,
                    to = Home1#location.id,
                    distance = Distance,
                    base_time = calculate_base_time(Distance)
                }
            ]
        end, ConnectedHomes)
    end, Homes).

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------

%% חישוב מרחק אוקלידי בין שתי נקודות
calculate_distance(Loc1, Loc2) ->
    DX = Loc1#location.x - Loc2#location.x,
    DY = Loc1#location.y - Loc2#location.y,
    round(math:sqrt(DX*DX + DY*DY)).

%% חישוב זמן נסיעה בסיסי לפי מרחק
%% מניח מהירות ממוצעת של 40 קמ"ש (11.11 מטר/שנייה)
calculate_base_time(Distance) ->
    round(Distance / 11.11).

%% הסרת כבישים כפולים
remove_duplicate_roads(Roads) ->
    RoadMap = lists:foldl(fun(Road, Map) ->
        %% המפתח תמיד יהיה הזוג הממוין של נקודות המוצא והיעד,
        %% כדי למנוע כבישים כפולים בין אותן שתי נקודות.
        Key = {min(Road#road.from, Road#road.to), max(Road#road.from, Road#road.to)},
        maps:put(Key, Road, Map)
    end, #{}, Roads),

    maps:values(RoadMap).

%% -----------------------------------------------------------
%% שמירה ב-ETS
%% -----------------------------------------------------------

%% שמירת המפה בטבלאות ETS
save_map_to_ets(Locations, Roads) ->
    io:format("Saving map data to ETS tables...~n"),

    %% יצירת טבלאות אם לא קיימות
    ensure_ets_tables(),

    %% ניקוי טבלאות קיימות
    ets:delete_all_objects(map_locations),
    ets:delete_all_objects(map_roads),
    ets:delete_all_objects(map_graph),

    %% שמירת לוקיישנים
    lists:foreach(fun(Loc) ->
        ets:insert(map_locations, {Loc#location.id, Loc})
    end, Locations),

    %% שמירת כבישים
    lists:foreach(fun(Road) ->
        ets:insert(map_roads, {Road#road.id, Road})
    end, Roads),

    %% בניית גרף לחיפוש מהיר
    build_graph_structure(Locations, Roads),

    io:format("Map data saved successfully~n").

%% וידוא שהטבלאות קיימות
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

%% --- התיקון הקריטי כאן ---
%% @doc בניית מבנה גרף לניווט מהיר, תוך התייחסות לכבישים כדו-כיווניים.
build_graph_structure(Locations, Roads) ->
    lists:foreach(fun(Loc) ->
        MyId = Loc#location.id,
        %% מחפשים את כל הכבישים שהמיקום הנוכחי הוא חלק מהם (או כמוצא או כיעד).
        RelatedRoads = [R || R <- Roads, R#road.from == MyId orelse R#road.to == MyId],

        Neighbors = lists:map(fun(Road) ->
            %% אם אני המוצא, השכן הוא היעד. אם אני היעד, השכן הוא המוצא.
            NeighborId = if
                Road#road.from == MyId -> Road#road.to;
                true -> Road#road.from
            end,
            {NeighborId, Road#road.distance, Road#road.base_time}
        end, RelatedRoads),

        ets:insert(map_graph, {MyId, Neighbors})
    end, Locations).
%% --- סוף התיקון ---

%% -----------------------------------------------------------
%% הדפסת סטטיסטיקות
%% -----------------------------------------------------------

print_map_statistics(Locations, Roads) ->
    NumHomes = length([L || L <- Locations, L#location.type == home]),
    NumBusinesses = length([L || L <- Locations, L#location.type == business]),

    %% ספירת בתים לפי אזור
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