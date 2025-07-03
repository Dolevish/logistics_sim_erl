%% -----------------------------------------------------------
%% מודול הסופרווייזר הראשי המשופר עם תמיכה במפה
%% מנהל את תהליכי התשתית כולל מודולי המפה החדשים
%% -----------------------------------------------------------

-module(logistics_sim_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, get_running_processes/0, restart_child/1]).

%% התחלת הסופרווייזר
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% אתחול הסופרווייזר והגדרת תהליכי התשתית
init([]) ->
    io:format("Main Supervisor starting with infrastructure processes...~n"),
    
    %% ChildSpecs – רשימת תהליכי התשתית כולל מודולי המפה
    ChildSpecs = [
        %% Map Server - חייב להיות ראשון כי אחרים תלויים בו
        #{id => map_server,
          start => {map_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [map_server]},

        %% Location Tracker - למעקב אחר תנועת שליחים
        #{id => location_tracker,
          start => {location_tracker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [location_tracker]},

        %% Control Center - המוח המרכזי שמנהל את כל הסימולציה
        #{id => control_center,
          start => {control_center, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [control_center]},

        %% State Collector - אוסף מידע מכל הרכיבים לממשק הגרפי
        #{id => logistics_state_collector,
          start => {logistics_state_collector, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [logistics_state_collector]},

        %% Web Server - שרת HTTP ו-WebSocket לממשק הגרפי
        #{id => logistics_web_server,
          start => {logistics_web_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [logistics_web_server]}
    ],
    
    %% אסטרטגיית הסופרווייזר
    %% one_for_one: אם תהליך אחד נכשל, רק הוא מופעל מחדש
    %% 5 כשלונות בתוך 60 שניות יגרמו לכשל של כל הסופרווייזר
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.

%% -----------------------------------------------------------
%% פונקציות עזר
%% -----------------------------------------------------------

%% קבלת רשימת כל התהליכים הפעילים
get_running_processes() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({Id, Pid, Type, Modules}) ->
        #{id => Id, 
          pid => Pid, 
          type => Type, 
          modules => Modules, 
          alive => is_process_alive(Pid)}
    end, Children).

%% הפעלת מחדש של תהליך ספציפי
restart_child(ChildId) ->
    case supervisor:terminate_child(?MODULE, ChildId) of
        ok ->
            supervisor:restart_child(?MODULE, ChildId);
        Error ->
            Error
    end.