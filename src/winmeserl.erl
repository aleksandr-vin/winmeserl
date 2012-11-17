-module(winmeserl).


%% API
-export([start/0, stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(winmeserl).

stop() ->
    application:stop(winmeserl).
