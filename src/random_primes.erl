-module(random_primes).

-export([get_statistics/0,
         flushdb/0,
         checks_primes_in_db/0]).

-spec get_statistics() -> #{number_of_primes => pos_integer(),
                            queue_length_of_random_numbers => pos_integer()}.
get_statistics() ->
    random_primes_lib:get_statistics().

-spec flushdb() ->
    {ok, any()} | {error, Reason::binary() | no_connection}.
flushdb() ->
    random_primes_lib:flushdb().

%%%===================================================================
%%% Tests
%%%===================================================================

-spec checks_primes_in_db() -> ok | {{not_primes, [binary()]}, {convertation_errors, [binary()]}}.
checks_primes_in_db() ->
    random_primes_filter:checks_primes_in_db().

