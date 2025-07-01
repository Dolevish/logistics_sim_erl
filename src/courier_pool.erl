%% -----------------------------------------------------------
%% מודול מנהל תור שליחים (Courier Pool)
%% מנהל תור FIFO גלובלי של שליחים פנויים
%% -----------------------------------------------------------
-module(courier_pool).
-behaviour(gen_server).

%% API
-export([start_link/0, request_courier/0, return_courier/1, get_queue_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

%% התחלת מנהל התור
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% בקשת שליח פנוי מהתור
request_courier() ->
    gen_server:call(?MODULE, request_courier).

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
        total_requests => 0,
        total_returns => 0
    }}.

%% טיפול בבקשת שליח
handle_call(request_courier, _From, State) ->
    Queue = maps:get(queue, State),
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
            io:format("Courier Pool: Assigning courier ~p (Queue now: ~p)~n", [Courier, Rest]),
            {reply, {ok, Courier}, NewState};
        [] ->
            %% אין שליחים פנויים
            io:format("Courier Pool: No couriers available!~n"),
            {reply, {error, no_couriers_available}, State}
    end;

%% קבלת סטטוס התור
handle_call(get_queue_status, _From, State) ->
    Status = #{
        available => maps:get(queue, State),
        busy => maps:get(busy_couriers, State),
        available_count => length(maps:get(queue, State)),
        busy_count => length(maps:get(busy_couriers, State)),
        total_requests => maps:get(total_requests, State),
        total_returns => maps:get(total_returns, State)
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% טיפול בהחזרת שליח לתור
handle_cast({return_courier, CourierId}, State) ->
    Queue = maps:get(queue, State),
    BusyCouriers = maps:get(busy_couriers, State),
    TotalReturns = maps:get(total_returns, State),
    
    %% בדיקה שהשליח באמת היה תפוס
    case lists:member(CourierId, BusyCouriers) of
        true ->
            %% הסר מרשימת התפוסים והוסף לסוף התור
            NewBusyCouriers = lists:delete(CourierId, BusyCouriers),
            NewQueue = Queue ++ [CourierId],
            NewState = State#{
                queue => NewQueue,
                busy_couriers => NewBusyCouriers,
                total_returns => TotalReturns + 1
            },
            io:format("Courier Pool: Courier ~p returned to queue (Queue now: ~p)~n", 
                     [CourierId, NewQueue]),
            {noreply, NewState};
        false ->
            %% השליח לא היה ברשימת התפוסים - בדוק אם הוא כבר בתור
            case lists:member(CourierId, Queue) of
                true ->
                    io:format("Courier Pool: Courier ~p already in queue, ignoring~n", [CourierId]),
                    {noreply, State};
                false ->
                    %% השליח לא נמצא בשום מקום - הוסף אותו לתור
                    io:format("Courier Pool: Adding lost courier ~p back to queue~n", [CourierId]),
                    NewQueue = Queue ++ [CourierId],
                    NewState = State#{
                        queue => NewQueue,
                        total_returns => TotalReturns + 1
                    },
                    {noreply, NewState}
            end
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