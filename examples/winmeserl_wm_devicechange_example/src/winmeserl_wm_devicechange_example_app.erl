-module(winmeserl_wm_devicechange_example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("log.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?info("Starting winmeserl_wm_devicechange_example!"),
    case winmeserl_wm_devicechange_handler:add_handler() of
        ok ->
            ?info("WM_DEVICECHANGE event handler added"),
            ok;
        E ->
            ?error("WM_DEVICECHANGE event handler not added: ~p", [E]),
            exit(E)
    end,
    winmeserl_wm_devicechange_example_sup:start_link().

stop(_State) ->
    ok.
