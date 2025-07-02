%% -----------------------------------------------------------
%% מודול מחולל הזמנות רנדומלי (Random Order Generator)
%% יוצר חבילה חדשה כל 5 שניות באזור רנדומלי
%% -----------------------------------------------------------
-module(random_order_generator).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([pause/0, resume/0, set_interval/1, get_stats/0]).

%% -----------------------------------------------------------
%% start_link/0 - מתחיל תהליך מחולל רנדומלי יחיד
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Random Order Generator started!~n"),
    %% אתחול מחולל המספרים הרנדומליים
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    %% רשימת האזורים
    Zones = ["north", "center", "south"],
    %% תזמון ישיר עם אינטרוול של 5 שניות
    DefaultInterval = 5000, %% 5 שניות
    erlang:send_after(DefaultInterval, self(), create_random_order),
    {ok, #{
        zones => Zones,
        next_id => 1, 
        active => true, 
        interval => DefaultInterval,
        total_orders => 0,
        %% מונה לכל אזור
        north_orders => 0,
        center_orders => 0,
        south_orders => 0
    }}.

%% -----------------------------------------------------------
%% יצירת הזמנה רנדומלית
%% -----------------------------------------------------------
handle_info(create_random_order, State) ->
    case maps:get(active, State) of
        true ->
            %% בחירת אזור רנדומלי
            Zones = maps:get(zones, State),
            RandomZone = lists:nth(rand:uniform(length(Zones)), Zones),
            
            %% יצירת ID ייחודי
            NextId = maps:get(next_id, State),
            PkgId = RandomZone ++ "_" ++ integer_to_list(NextId),
            
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
                    io:format("=== Order Stats: Total: ~p, North: ~p, Center: ~p, South: ~p ===~n", 
                             [maps:get(total_orders, NewState),
                              maps:get(north_orders, NewState),
                              maps:get(center_orders, NewState),
                              maps:get(south_orders, NewState)]);
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
    Stats = #{
        active => maps:get(active, State),
        total_orders => maps:get(total_orders, State),
        north_orders => maps:get(north_orders, State),
        center_orders => maps:get(center_orders, State),
        south_orders => maps:get(south_orders, State),
        next_id => maps:get(next_id, State),
        interval => maps:get(interval, State)
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