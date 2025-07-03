%% -----------------------------------------------------------
%% מודול הסופרווייזר הראשי המשופר
%% מנהל רק את תהליכי התשתית - Control Center, State Collector, Web Server
%% תהליכי הסימולציה (אזורים, שליחים, מחולל הזמנות) יופעלו דינמית
%% -----------------------------------------------------------

-module(logistics_sim_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, get_running_processes/0, restart_child/1]).

%% התחלת הסופרווייזר
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% אתחול הסופרווייזר והגדרת תהליכי התשתית בלבד
init([]) ->
    io:format("Main Supervisor starting with infrastructure processes only...~n"),
    
    %% ChildSpecs – רשימת תהליכי התשתית בלבד
    ChildSpecs = [
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