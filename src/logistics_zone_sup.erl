%% -----------------------------------------------------------
%% מודול הסופרווייזר למנהלי אזורים - Zone Managers
%% מנהל את מנהלי האזורים, מאגר השליחים והמרכיבים הקשורים
%% -----------------------------------------------------------

-module(logistics_zone_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% התחלת הסופרווייזר
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% אתחול הסופרווייזר והגדרת תהליכי האזורים
init([]) ->
    io:format("Zone Supervisor starting...~n"),
    
    %% קבלת רשימת האזורים מההגדרות
    ZoneIds = application:get_env(logistics_sim, zone_ids, [1, 2, 3]),
    ZoneNames = [integer_to_list(Id) || Id <- ZoneIds],
    
    %% ChildSpecs – מאגר שליחים ומנהלי אזורים
    ChildSpecs = [
        %% Courier Pool - מאגר השליחים
        #{id => courier_pool,
          start => {courier_pool, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [courier_pool]},

        %% Location Tracker - למעקב אחר תנועת שליחים
        #{id => location_tracker,
          start => {location_tracker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [location_tracker]}
    ] ++
    %% יצירת מנהל אזור לכל אזור שהוגדר
    [#{id => list_to_atom("zone_manager_" ++ ZoneName),
       start => {zone_manager, start_link, [ZoneName]},
       restart => permanent,
       shutdown => 5000,
       type => worker,
       modules => [zone_manager]} || ZoneName <- ZoneNames],
    
    %% אסטרטגיית הסופרווייזר
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.
