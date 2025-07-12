%% -----------------------------------------------------------
%% מודול סופרווייזר דינמי לסימולציה
%% מנהל את כל תהליכי הסימולציה שנוצרים ונהרסים דינמית
%% -----------------------------------------------------------
-module(simulation_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% התחלת הסופרווייזר
start_link() ->
    supervisor:start_link({via, global, ?MODULE}, ?MODULE, []).

%% אתחול הסופרווייזר
init([]) ->
    io:format("Simulation Supervisor starting...~n"),
    
    %% הגדרת אסטרטגיית הסופרווייזר
    %% one_for_one - אם תהליך נופל, רק הוא מופעל מחדש
    %% 10 כשלונות בתוך 60 שניות יגרמו לכשל של הסופרווייזר
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% התחלה עם רשימה ריקה - תהליכים יתווספו דינמית
    ChildSpecs = [],
    
    {ok, {SupFlags, ChildSpecs}}.