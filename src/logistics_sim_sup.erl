%% -----------------------------------------------------------
%% מודול הסופרווייזר של המערכת (logistics_sim_sup)
%% מנהל את כל תהליכי הליבה – Control Center, אזורים, שליחים, Order Generator
%% תיקון: עדכון הערות לגבי הקצאת שליחים לאזורים
%% -----------------------------------------------------------

-module(logistics_sim_sup).
-behaviour(supervisor).

%% הוספתי exports נדרשים - כולל הפונקציות העזר
-export([start_link/0, init/1, get_running_processes/0, restart_child/1]).

%% התחלת הסופרווייזר
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% אתחול הסופרווייזר והגדרת כל התהליכים
init([]) ->
    %% ChildSpecs – רשימת כל התהליכים שהסופרווייזר מפעיל
    %% שיפור: ארגנתי את התהליכים בסדר עדיפות לוגי
    ChildSpecs = [
        %% Control Center (תהליך שליטה ראשי) - עדיפות גבוהה
        #{id => control_center,
          start => {control_center, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [control_center]},

        %% Courier Pool Manager - מנהל תור השליחים המרכזי
        %% חייב להיות לפני Zone Managers
        #{id => courier_pool,
          start => {courier_pool, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier_pool]},

        %% שלושה Zone Managers (צפון, מרכז, דרום) - עדיפות גבוהה
        #{id => zone_manager_north,
          start => {zone_manager, start_link, ["north"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [zone_manager]},
        #{id => zone_manager_center,
          start => {zone_manager, start_link, ["center"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [zone_manager]},
        #{id => zone_manager_south,
          start => {zone_manager, start_link, ["south"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [zone_manager]},

        %% שליחים - כל השליחים זמינים לכל האזורים
        %% השליחים יכולים לקחת משלוחים מכל אזור במערכת
        #{id => courier1,
          start => {courier, start_link, ["courier1"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier2,
          start => {courier, start_link, ["courier2"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier3,
          start => {courier, start_link, ["courier3"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier4,
          start => {courier, start_link, ["courier4"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier5,
          start => {courier, start_link, ["courier5"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier6,
          start => {courier, start_link, ["courier6"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier7,
          start => {courier, start_link, ["courier7"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},
        #{id => courier8,
          start => {courier, start_link, ["courier8"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier]},

        %% Random Order Generator - מחולל הזמנות רנדומלי יחיד
        %% יוצר חבילה חדשה כל 15 שניות באזור רנדומלי
        #{id => random_order_generator,
          start => {random_order_generator, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [random_order_generator]}

        %% TODO: בעתיד אפשר להוסיף כאן:
        %% - Web Interface Node - ממשק ווב לניהול המערכת
        %% - Visualization Node - הצגה גרפית של המערכת בזמן אמת
        %% - Metrics Collector - איסוף נתונים סטטיסטיים
        %% - Load Balancer - איזון עומסים בין אזורים
        %% - Household Processes - תהליכי משקי בית שמייצרים הזמנות
    ],
    
    %% שיפור: שיניתי את אסטרטגיית הסופרווייזר להיות יותר סלחנית
    %% one_for_one: אם תהליך אחד נכשל, רק הוא מופעל מחדש
    %% 5 כשלונות בתוך 60 שניות יגרמו לכשל של כל הסופרווייזר
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.

%% -----------------------------------------------------------
%% פונקציות עזר נוספות (אופציונליות)
%% -----------------------------------------------------------

%% הוספתי פונקציה לקבלת רשימת כל התהליכים הפעילים
%% מחזירה מפה עם פרטי כל התהליכים: id, pid, type, modules, והאם חי
get_running_processes() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({Id, Pid, Type, Modules}) ->
        #{id => Id, 
          pid => Pid, 
          type => Type, 
          modules => Modules, 
          alive => is_process_alive(Pid)}
    end, Children).

%% הוספתי פונקציה להפעלת מחדש של תהליך ספציפי
%% מקבלת את ה-ID של התהליך ומפעילה אותו מחדש
restart_child(ChildId) ->
    case supervisor:terminate_child(?MODULE, ChildId) of
        ok ->
            %% הצלחנו לסגור את התהליך - נפעיל מחדש
            supervisor:restart_child(?MODULE, ChildId);
        Error ->
            %% נכשל - מחזיר את השגיאה
            Error
    end.