%% -----------------------------------------------------------
%% מודול מחולל הזמנות (Order Generator)
%% יוצר כל כמה שניות משלוח חדש לאזור מסוים
%% -----------------------------------------------------------
-module(order_generator).
-behaviour(gen_server).

%% הוספתי exports לכל הפונקציות הנדרשות
-export([start_link/1, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% הוספתי פונקציות API נוספות לשליטה במחולל
-export([pause/1, resume/1, set_interval/2, get_stats/1]).

%% -----------------------------------------------------------
%% start_link/1 - מתחיל תהליך מחולל לאזור מסוים
%% -----------------------------------------------------------
start_link(Zone) ->
    gen_server:start_link({local, list_to_atom("order_gen_" ++ Zone)}, ?MODULE, [Zone], []).

init([Zone]) ->
    io:format("Order Generator for zone ~p started!~n", [Zone]),
    %% תיקון קריטי: תזמון ישיר עם אינטרוול קבוע במקום קריאה לפונקציה שגורמת לdeadlock
    DefaultInterval = 30000, %% 30 שניות ברירת מחדל
    erlang:send_after(DefaultInterval, self(), {new_order, Zone, 1}),
    {ok, #{
        zone => Zone, 
        next_id => 2, 
        active => true, 
        interval => DefaultInterval,
        total_orders => 0   %% מספר כולל של הזמנות שנוצרו
    }}.

%% -----------------------------------------------------------
%% לייצר כל כמה שניות משלוח חדש (חבילה חדשה)
%% הוספתי בדיקה אם המחולל פעיל
%% -----------------------------------------------------------
handle_info({new_order, Zone, Id}, State) ->
    case maps:get(active, State) of
        true ->
            PkgId = Zone ++ "_" ++ integer_to_list(Id),
            io:format("Order Generator: Creating package ~p for zone ~p~n", [PkgId, Zone]),
            %% שליחת חבילה חדשה ל-zone_manager
            zone_manager:new_package(Zone, PkgId),
            %% תיקון: תזמון ישיר עם האינטרוול מה-state
            Interval = maps:get(interval, State),
            erlang:send_after(Interval, self(), {new_order, Zone, Id + 1}),
            %% עדכון סטטיסטיקות
            TotalOrders = maps:get(total_orders, State, 0),
            {noreply, State#{next_id => Id + 1, total_orders => TotalOrders + 1}};
        false ->
            %% אם המחולל לא פעיל, נתזמן שוב בעוד דקה
            erlang:send_after(60000, self(), {new_order, Zone, Id}),
            {noreply, State}
    end;

handle_info(_, State) -> {noreply, State}.

%% הוספתי פונקציות API לשליטה במחולל
handle_call({pause}, _From, State) ->
    io:format("Order Generator paused for zone ~p~n", [maps:get(zone, State)]),
    {reply, ok, State#{active => false}};

handle_call({resume}, _From, State) ->
    Zone = maps:get(zone, State),
    NextId = maps:get(next_id, State),
    Interval = maps:get(interval, State),
    io:format("Order Generator resumed for zone ~p~n", [Zone]),
    %% תזמון הזמנה חדשה מיד עם האינטרוול הנוכחי
    erlang:send_after(Interval, self(), {new_order, Zone, NextId}),
    {reply, ok, State#{active => true}};

handle_call({set_interval, NewInterval}, _From, State) ->
    io:format("Order Generator interval changed to ~p ms for zone ~p~n", [NewInterval, maps:get(zone, State)]),
    {reply, ok, State#{interval => NewInterval}};

%% תיקון: הסרתי את הקריאה ל-get_interval שגרמה לבעיה
handle_call({get_stats}, _From, State) ->
    Stats = #{
        zone => maps:get(zone, State),
        active => maps:get(active, State),
        total_orders => maps:get(total_orders, State),
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
pause(Zone) ->
    gen_server:call(list_to_atom("order_gen_" ++ Zone), {pause}).

resume(Zone) ->
    gen_server:call(list_to_atom("order_gen_" ++ Zone), {resume}).

set_interval(Zone, IntervalMs) ->
    gen_server:call(list_to_atom("order_gen_" ++ Zone), {set_interval, IntervalMs}).

get_stats(Zone) ->
    gen_server:call(list_to_atom("order_gen_" ++ Zone), {get_stats}).