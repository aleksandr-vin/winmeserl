%%% -------------------------------------------------------------------
%%% File    : log.hrl
%%% Author  : Denis Sizov <denis.sizov@gmail.com>
%%% Description: Logger interface
%%%
%%% Created : Jun 18, 2012 by Denis Sizov <denis.sizov@gmail.com>
%%% @copyright 2012 Peter-Service
%%% -------------------------------------------------------------------

-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-compile([{parse_transform, lager_transform}]).


%% Logging wrappers
-define(debug(Tags, Fmt, Data),    lager:debug(Tags, Fmt, Data)).
-define(info(Tags, Fmt, Data),     lager:info(Tags, Fmt, Data)).
-define(notice(Tags, Fmt, Data),   lager:notice(Tags, Fmt, Data)).
-define(warning(Tags, Fmt, Data),  lager:warning(Tags, Fmt, Data)).
-define(error(Tags, Fmt, Data),    lager:error(Tags, Fmt, Data)).
-define(critical(Tags, Fmt, Data), lager:critical(Tags, Fmt, Data)).
-define(alert(Tags, Fmt, Data),    lager:alert(Tags, Fmt, Data)).

-define(debug(Fmt, Data),          lager:debug(Fmt, Data)).
-define(info(Fmt, Data),           lager:info(Fmt, Data)).
-define(notice(Fmt, Data),         lager:notice(Fmt, Data)).
-define(warning(Fmt, Data),        lager:warning(Fmt, Data)).
-define(error(Fmt, Data),          lager:error(Fmt, Data)).
-define(critical(Fmt, Data),       lager:critical(Fmt, Data)).
-define(alert(Fmt, Data),          lager:alert(Fmt, Data)).

-define(debug(Msg),                ?debug(Msg, [])).
-define(info(Msg),                 ?info(Msg, [])).
-define(notice(Msg),               ?notice(Msg, [])).
-define(warning(Msg),              ?warning(Msg, [])).
-define(error(Msg),                ?error(Msg, [])).
-define(critical(Msg),             ?critical(Msg, [])).
-define(alert(Msg),                ?alert(Msg, [])).

%% Assertion + logging
-define(assertlog(Msg, Pattern, What),
        try Pattern = What of
            _ -> true
        catch
            badmatch ->
                ?error("Assertion failed: ~p - ~p", [Msg, ??Pattern]),
                error({assert_failed, Msg, Pattern});
            _ ->
                ?error("Assertion failed (exception): ~p - ~p", [Msg, ??Pattern]),
                error({assert_failed, Msg, exception})
        end
).

-define(assertlog(Msg, What), ?assertlog(Msg, true, What)).


-endif.

