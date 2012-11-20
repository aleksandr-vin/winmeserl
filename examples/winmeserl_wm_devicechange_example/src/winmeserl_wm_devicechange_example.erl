-module(winmeserl_wm_devicechange_example).

%% Application callbacks
-export([start/0, stop/0]).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).
