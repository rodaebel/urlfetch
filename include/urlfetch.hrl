%% The default port
-define(PORT, 10190).

%% Close client connections after 10 seconds idle
-define(TIMEOUT, 10000).

-define(MAX_RESTART, 5).

-define(MAX_TIME, 60).

-define(RETRY_COUNT, 1).

-define(RETRY_TIMEOUT, 5000).

-record(cache, {id, status_code=200, data, complete, timestamp}).

-define(USER_AGENT, "Urlfetch/1.0").

-define(EMPTY, <<"">>).
