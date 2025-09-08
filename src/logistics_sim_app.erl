%%%-------------------------------------------------------------------
%% @doc logistics_sim public API
%% @end
%%%-------------------------------------------------------------------

-module(logistics_sim_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% קריאת הגדרות התפקיד של הנוד הנוכחי
    Role = application:get_env(logistics_sim, role, control_center),
    io:format("Starting node with role: ~p~n", [Role]),
    
    %% הגדרת cookie לתקשורת בין נודים (אם לא הוגדר)
    case application:get_env(logistics_sim, node_cookie) of
        {ok, Cookie} ->
            erlang:set_cookie(node(), Cookie);
        undefined ->
            erlang:set_cookie(node(), logistics_cookie)
    end,
    
    %% התחברות לנודים אחרים בהתאם לתפקיד
    connect_to_required_nodes(Role),
    
    %% התחלת הסופרווייזר בהתאם לתפקיד
    case Role of
        control_center ->
            logistics_sim_sup:start_link();
        ui ->
            logistics_ui_sup:start_link();
        zone_manager ->
            logistics_zone_sup:start_link();
        _ ->
            {error, {unknown_role, Role}}
    end.

stop(_State) ->
    ok.

%% פונקציות פנימיות

%% התחברות לנודים נדרשים בהתאם לתפקיד הנוד
connect_to_required_nodes(control_center) ->
    %% מרכז הבקרה מתחבר לכל הנודים האחרים
    UINode = application:get_env(logistics_sim, ui_node, 'ui@192.168.64.3'),
    ZoneNodes = application:get_env(logistics_sim, zone_nodes, []),
    connect_to_nodes([UINode | ZoneNodes]);

connect_to_required_nodes(ui) ->
    %% UI מתחבר רק למרכז הבקרה
    ControlNode = application:get_env(logistics_sim, control_node, 'control@192.168.64.3'),
    connect_to_nodes([ControlNode]);

connect_to_required_nodes(zone_manager) ->
    %% מנהל אזור מתחבר רק למרכז הבקרה
    ControlNode = application:get_env(logistics_sim, control_node, 'control@192.168.64.3'),
    connect_to_nodes([ControlNode]);

connect_to_required_nodes(_) ->
    ok.

%% התחברות לרשימת נודים
connect_to_nodes(Nodes) ->
    lists:foreach(fun(Node) ->
        case net_kernel:connect_node(Node) of
            true ->
                io:format("Successfully connected to node: ~p~n", [Node]);
            false ->
                io:format("Failed to connect to node: ~p~n", [Node])
        end
    end, Nodes).

%% internal functions