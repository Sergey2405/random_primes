-define(APP, random_primes).
-define(SUPERVISOR, random_primes_sup).
-define(EREDIS, eredis).

-define(LOCAL_HOST, "127.0.0.1").

-define(EREDIS_PORT, 6379).
-define(EREDIS_DEFAULT_DB, 0).

-define(PRIME_RANGE, 1000000).
-define(RATE_PER_SECOND, 3000).

-define(MAX_FILTER_PROCESSES, 1000).
-define(MIN_FILTER_DYNAMIC_INTERVAL, 1000).%ms
-define(MAX_FILTER_DYNAMIC_INTERVAL, 60000).%ms

-define(EUNIT, eunit).

-define(SPEC_WORKER(Id), #{id => Id, start => {Id, start_link, []}}).
-define(SPEC_WORKER(Id, Args), #{id => Id, start => {Id, start_link, Args}}).
-define(SPEC_WORKER(Id, M, Args), #{id => Id, start => {M, start_link, Args}}).
-define(SPEC_WORKER(Id, M, F, Args), #{id => Id, start => {M, F, Args}}).

-define(SPEC_SUPERVISOR(Id), #{id => Id, start => {Id, start_link, []},type => supervisor}).
