%% -----------------------------------------------------------
%% מודול מחולל הזמנות רנדומלי משופר - תומך במיקומים על המפה
%% יוצר חבילה חדשה כל X שניות באזור רנדומלי עם בית ספציפי
%% -----------------------------------------------------------
-module(random_order_generator).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([pause/0, resume/0, set_interval/1, get_stats/0]).

%% הגדרת האזורים הקבועים
-define(FIXED_ZONES, ["north", "center", "south"]).

%% -----------------------------------------------------------
%% start_link/0 - מתחיל תהליך מחולל רנדומלי יחיד
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Random Order Generator started with fixed zones: ~p~n", [?FIXED_ZONES]),
    %% אתחול מחולל המספרים הרנדומליים
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    
    %% קריאת interval מההגדרות
    DefaultInterval = get_interval_from_config(),
    
    %% בדיקה אם המפה מופעלת
    MapEnabled = case ets:info(simulation_config) of
        undefined -> false;
        _ ->
            case ets:lookup(simulation_config, map_enabled) of
                [{map_enabled, true}] -> true;
                _ -> false
            end
    end,
    
    io:format("Random Order Generator: Using fixed zones ~p with interval ~p ms (Map enabled: ~p)~n", 
              [?FIXED_ZONES, DefaultInterval, MapEnabled]),
    
    %% תזמון ישיר
    erlang:send_after(DefaultInterval, self(), create_random_order),
    
    %% יצירת מפה ריקה למונים לכל אזור קבוע
    ZoneCounters = lists:foldl(fun(Zone, Acc) ->
        maps:put(list_to_atom(Zone ++ "_orders"), 0, Acc)
    end, #{}, ?FIXED_ZONES),
    
    State = maps:merge(#{
        zones => ?FIXED_ZONES,  %% שימוש באזורים הקבועים
        next_id => 1, 
        active => true, 
        interval => DefaultInterval,
        total_orders => 0,
        map_enabled => MapEnabled  %% שמירת מצב המפה
    }, ZoneCounters),
    
    {ok, State}.

%% -----------------------------------------------------------
%% יצירת הזמנה רנדומלית
%% -----------------------------------------------------------
handle_info(create_random_order, State) ->
    case maps:get(active, State) of
        true ->
            %% בחירת אזור רנדומלי מתוך האזורים הקבועים
            Zones = ?FIXED_ZONES,
            RandomZone = lists:nth(rand:uniform(length(Zones)), Zones),
            
            %% יצירת ID ייחודי
            NextId = maps:get(next_id, State),
            MapEnabled = maps:get(map_enabled, State, false),
            
            %% יצירת ID של החבילה בהתאם למצב המפה
            %% תיקון: טיפול בכל המקרים האפשריים
            PkgId = case MapEnabled of
                true ->
                    %% עם מפה - נסה לקבל בית ספציפי
                    case map_server:get_random_home_in_zone(list_to_atom(RandomZone)) of
                        {ok, Home} ->
                            HomeId = extract_home_id(Home),
                            RandomZone ++ "_order_" ++ integer_to_list(NextId) ++ "_to_" ++ HomeId;
                        {error, Reason} ->
                            io:format("Random Order Generator: Failed to get random home in zone ~p: ~p~n", 
                                     [RandomZone, Reason]),
                            %% נפול בחזרה ל-ID פשוט
                            RandomZone ++ "_" ++ integer_to_list(NextId)
                    end;
                _ ->
                    %% בלי מפה או כל ערך אחר - ID פשוט
                    RandomZone ++ "_" ++ integer_to_list(NextId)
            end,
            
            io:format("Random Order Generator: Creating package ~p for zone ~p~n", [PkgId, RandomZone]),
            
            %% שליחת חבילה חדשה ל-zone_manager
            zone_manager:new_package(RandomZone, PkgId),
            
            %% תזמון ההזמנה הבאה
            Interval = maps:get(interval, State),
            erlang:send_after(Interval, self(), create_random_order),
            
            %% עדכון סטטיסטיקות
            TotalOrders = maps:get(total_orders, State, 0),
            ZoneKey = list_to_atom(RandomZone ++ "_orders"),
            ZoneOrders = maps:get(ZoneKey, State, 0),
            
            NewState = State#{
                next_id => NextId + 1, 
                total_orders => TotalOrders + 1,
                ZoneKey => ZoneOrders + 1
            },
            
            %% הדפסת סטטיסטיקות כל 10 הזמנות
            case (TotalOrders + 1) rem 10 of
                0 ->
                    print_stats(NewState);
                _ -> ok
            end,
            
            {noreply, NewState};
        false ->
            %% אם המחולל לא פעיל, נתזמן שוב בעוד דקה
            erlang:send_after(60000, self(), create_random_order),
            {noreply, State}
    end;

handle_info(_, State) -> {noreply, State}.

%% API לשליטה במחולל
handle_call(pause, _From, State) ->
    io:format("Random Order Generator paused~n"),
    {reply, ok, State#{active => false}};

handle_call(resume, _From, State) ->
    io:format("Random Order Generator resumed~n"),
    Interval = maps:get(interval, State),
    %% תזמון הזמנה חדשה מיד
    erlang:send_after(Interval, self(), create_random_order),
    {reply, ok, State#{active => true}};

handle_call({set_interval, NewInterval}, _From, State) ->
    io:format("Random Order Generator interval changed to ~p ms~n", [NewInterval]),
    {reply, ok, State#{interval => NewInterval}};

handle_call(get_stats, _From, State) ->
    %% בניית מפת סטטיסטיקות דינמית עבור האזורים הקבועים
    Zones = ?FIXED_ZONES,
    ZoneStats = lists:foldl(fun(Zone, Acc) ->
        ZoneKey = list_to_atom(Zone ++ "_orders"),
        maps:put(Zone, maps:get(ZoneKey, State, 0), Acc)
    end, #{}, Zones),
    
    Stats = #{
        active => maps:get(active, State),
        total_orders => maps:get(total_orders, State),
        zone_stats => ZoneStats,
        next_id => maps:get(next_id, State),
        interval => maps:get(interval, State),
        map_enabled => maps:get(map_enabled, State, false)
    },
    {reply, Stats, State};

handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% -----------------------------------------------------------
%% API functions - פונקציות נוחות לשליטה במחולל מבחוץ
%% -----------------------------------------------------------
pause() ->
    gen_server:call(?MODULE, pause).

resume() ->
    gen_server:call(?MODULE, resume).

set_interval(IntervalMs) ->
    gen_server:call(?MODULE, {set_interval, IntervalMs}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% -----------------------------------------------------------
%% פונקציות עזר פרטיות
%% -----------------------------------------------------------

%% קריאת interval מההגדרות
get_interval_from_config() ->
    case ets:info(simulation_config) of
        undefined -> 
            5000;  % ברירת מחדל - 5 שניות
        _ ->
            case ets:lookup(simulation_config, order_interval) of
                [{order_interval, Interval}] -> Interval;
                [] -> 5000  % ברירת מחדל
            end
    end.

%% חילוץ מזהה הבית מאובייקט הלוקיישן
extract_home_id(Home) ->
    case Home of
        {location, Id, _, _, _, _, _} -> Id;
        #{id := Id} -> Id;
        _ -> "unknown"
    end.

%% הדפסת סטטיסטיקות
print_stats(State) ->
    Zones = ?FIXED_ZONES,
    TotalOrders = maps:get(total_orders, State),
    
    io:format("=== Order Stats: Total: ~p ===~n", [TotalOrders]),
    lists:foreach(fun(Zone) ->
        ZoneKey = list_to_atom(Zone ++ "_orders"),
        ZoneOrders = maps:get(ZoneKey, State, 0),
        io:format("  ~s: ~p~n", [Zone, ZoneOrders])
    end, Zones).