%% -----------------------------------------------------------
%% מודול מנהל תור שליחים (Courier Pool) - מתוקן עם Registry
%% מנהל תור FIFO גלובלי של שליחים פנויים
%% וגם תור של אזורים הממתינים לשליח
%% -----------------------------------------------------------
-module(courier_pool).
-behaviour(gen_server).

%% API - נוספה פונקציה לבקשת שליח עם ציון האזור המבקש
-export([start_link/0, request_courier/1, return_courier/1, get_queue_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

%% התחלת מנהל התור
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% בקשת שליח פנוי מהתור (עם ציון האזור המבקש)
%% ZoneName הוא string
request_courier(ZoneName) ->
    gen_server:call(?MODULE, {request_courier, ZoneName}).

%% החזרת שליח לתור (לסוף התור)
return_courier(CourierId) ->
    gen_server:cast(?MODULE, {return_courier, CourierId}).

%% קבלת סטטוס התור
get_queue_status() ->
    gen_server:call(?MODULE, get_queue_status).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Courier Pool Manager starting...~n"),
    %% אתחול עם כל השליחים בתור
    InitialQueue = ["courier1", "courier2", "courier3", "courier4",
                    "courier5", "courier6", "courier7", "courier8"],
    io:format("Courier Pool initialized with queue: ~p~n", [InitialQueue]),
    {ok, #{
        queue => InitialQueue,
        busy_couriers => [],
        %% שינוי: הוספת תור לאזורים שממתינים לשליח
        waiting_zones => queue:new(),
        total_requests => 0,
        total_returns => 0
    }}.

%% טיפול בבקשת שליח - מתוקן כדי לנהל תור המתנה
handle_call({request_courier, ZoneName}, From, State) ->
    Queue = maps:get(queue, State),
    {FromPid, _} = From,
    case Queue of
        [Courier | Rest] ->
            %% יש שליח פנוי - הוצא אותו מהתור
            BusyCouriers = maps:get(busy_couriers, State),
            TotalRequests = maps:get(total_requests, State),
            NewState = State#{
                queue => Rest,
                busy_couriers => [Courier | BusyCouriers],
                total_requests => TotalRequests + 1
            },
            io:format("Courier Pool: Assigning courier ~p to zone ~p (Queue now: ~p)~n", [Courier, ZoneName, Rest]),
            {reply, {ok, Courier}, NewState};
        [] ->
            %% אין שליחים פנויים - הוסף את האזור המבקש לתור ההמתנה
            io:format("Courier Pool: No couriers available! Zone ~p added to waiting queue.~n", [ZoneName]),
            WaitingZones = maps:get(waiting_zones, State),
            NewWaitingZones = queue:in({ZoneName, FromPid}, WaitingZones),
            NewState = State#{waiting_zones => NewWaitingZones},
            {reply, {error, no_couriers_available}, NewState}
    end;

%% קבלת סטטוס התור
handle_call(get_queue_status, _From, State) ->
    Status = #{
        available => maps:get(queue, State),
        busy => maps:get(busy_couriers, State),
        %% שינוי: הוספת מידע על אזורים ממתינים
        waiting_zones => queue:to_list(maps:get(waiting_zones, State)),
        available_count => length(maps:get(queue, State)),
        busy_count => length(maps:get(busy_couriers, State)),
        total_requests => maps:get(total_requests, State),
        total_returns => maps:get(total_returns, State)
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% טיפול בהחזרת שליח לתור - מתוקן כדי לבדוק אם יש אזורים ממתינים
handle_cast({return_courier, CourierId}, State) ->
    BusyCouriers = maps:get(busy_couriers, State),
    TotalReturns = maps:get(total_returns, State),
    WaitingZones = maps:get(waiting_zones, State),

    %% הסרת השליח מרשימת התפוסים
    NewBusyCouriers = lists:delete(CourierId, BusyCouriers),

    case queue:out(WaitingZones) of
        {{value, {ZoneName, ZonePid}}, NewWaitingZones} ->
            %% שינוי: יש אזור שממתין! שלח אליו ישירות את השליח
            io:format("Courier Pool: Courier ~p returned and immediately assigned to waiting zone ~p~n", [CourierId, ZoneName]),
            gen_statem:cast(ZonePid, {assign_to_waiting_package, CourierId}),
            %% השליח עובר ישירות לאזור, לא נכנס לתור הפנויים
            NewState = State#{
                busy_couriers => NewBusyCouriers,
                waiting_zones => NewWaitingZones,
                total_returns => TotalReturns + 1
            },
            {noreply, NewState};

        {empty, _} ->
            %% אין אזורים ממתינים, החזר את השליח לתור הפנויים
            Queue = maps:get(queue, State),
            NewQueue = Queue ++ [CourierId], % הוספה לסוף התור
            io:format("Courier Pool: Courier ~p returned to queue (Queue now: ~p)~n", [CourierId, NewQueue]),
            NewState = State#{
                queue => NewQueue,
                busy_couriers => NewBusyCouriers,
                total_returns => TotalReturns + 1
            },
            {noreply, NewState}
    end;


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Courier Pool Manager terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.