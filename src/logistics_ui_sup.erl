%% -----------------------------------------------------------
%% מודול הסופרווייזר לנוד UI - ממשק המשתמש והוויזואליזציה
%% מנהל רק את שרת הווב ו-WebSocket
%% -----------------------------------------------------------

-module(logistics_ui_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% התחלת הסופרווייזר
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% אתחול הסופרווייזר והגדרת תהליכי ה-UI
init([]) ->
    io:format("UI Supervisor starting...~n"),
    
    %% ChildSpecs – רק שרת הווב עבור UI
    ChildSpecs = [
        %% Web Server - שרת HTTP ו-WebSocket לממשק הגרפי
        #{id => logistics_web_server,
          start => {logistics_web_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [logistics_web_server]}
    ],
    
    %% אסטרטגיית הסופרווייזר
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.
