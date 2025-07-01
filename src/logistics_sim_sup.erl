%% -----------------------------------------------------------
%% מודול הסופרווייזר של המערכת (logistics_sim_sup)
%% מנהל את כל תהליכי הליבה – Control Center, אזורים, שליחים, Order Generator
%% -----------------------------------------------------------

-module(logistics_sim_sup).
-behaviour(supervisor).

%% הוספתי exports נדרשים - כולל הפונקציות העזר
-export([start_link/0, init/1, get_running_processes/0, restart_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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

        %% שליחים לאזור הצפון - עדיפות בינונית
        %% הוספתי שליחים נוספים ופיזור בין אזורים
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

        %% הוספתי שליחים לאזור המרכז
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

        %% הוספתי שליחים לאזור הדרום
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

        %% Order Generators לכל האזורים - עדיפות נמוכה
        %% הוספתי מחוללי הזמנות לכל האזורים במקום רק לצפון
        #{id => order_generator_north,
          start => {order_generator, start_link, ["north"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [order_generator]},
        #{id => order_generator_center,
          start => {order_generator, start_link, ["center"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [order_generator]},
        #{id => order_generator_south,
          start => {order_generator, start_link, ["south"]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [order_generator]}

        %% TODO: בעתיד אפשר להוסיף כאן:
        %% - Web Interface Node
        %% - Visualization Node  
        %% - Metrics Collector
        %% - Load Balancer
    ],
    
    %% שיפור: שיניתי את אסטרטגיית הסופרווייזר להיות יותר סלחנית
    %% one_for_one: אם תהליך אחד נכשל, רק הוא מופעל מחדש
    %% 5 כשלונות בתוך 60 שניות יגרמו לכשל של כל הסופרווייזר
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.

%% -----------------------------------------------------------
%% פונקציות עזר נוספות (אופציונליות)
%% -----------------------------------------------------------

%% הוספתי פונקציה לקבלת רשימת כל התהליכים הפעילים
get_running_processes() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({Id, Pid, Type, Modules}) ->
        #{id => Id, pid => Pid, type => Type, modules => Modules, alive => is_process_alive(Pid)}
    end, Children).

%% הוספתי פונקציה להפעלת מחדש של תהליך ספציפי
restart_child(ChildId) ->
    case supervisor:terminate_child(?MODULE, ChildId) of
        ok ->
            supervisor:restart_child(?MODULE, ChildId);
        Error ->
            Error
    end.