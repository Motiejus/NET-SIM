-ifndef(LOG_UTILS__HRL).
-define(LOG_UTILS__HRL, true).

-define(source_pos, ?FILE ++ ":" ++ integer_to_list(?LINE)).

-define(error(M, Opts),
    error_logger:error_report
        ([{pid, self()}|[{source, ?source_pos}|[{message, M}|Opts]]])).
-define(warning(M, Opts),
    error_logger:warning_report
        ([{pid, self()}|[{source, ?source_pos}|[{message, M}|Opts]]])).
-define(info(M, Opts),
    error_logger:info_report
        ([{pid, self()}|[{source, ?source_pos}|[{message, M}|Opts]]])).

-define(mute_log(), error_logger:tty(false)).
-define(unmute_log(), error_logger:tty(true)).

-endif.
