%%%-------------------------------------------------------------------
%% @doc random_primes top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-include("random_primes.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},

    EredisHost = random_primes_lib:get_env(?EREDIS, host, ?LOCAL_HOST),
    EredisPort = random_primes_lib:get_env(?EREDIS, port, ?EREDIS_PORT),
    EredisDB = random_primes_lib:get_env(?EREDIS, database, ?EREDIS_DEFAULT_DB),

    ChildSpecs = [?SPEC_WORKER(eredis, [EredisHost, EredisPort, EredisDB])],
    ChildSpecs2 = case random_primes_lib:get_env(?APP, generator) of
                    undefined -> ChildSpecs;
                    MapValue when is_map(MapValue) ->
                        RatePerSecond = random_primes_lib:get_env(?APP, generator, rate_per_second, ?RATE_PER_SECOND),
                        PrimeRange = random_primes_lib:get_env(?APP, generator, prime_range, ?RATE_PER_SECOND),
                        [?SPEC_WORKER(random_primes_gen, [PrimeRange, RatePerSecond])|ChildSpecs]
                  end,
    ChildSpecs3 = case random_primes_lib:get_env(?APP, filter) of
                    undefined -> ChildSpecs2;
                    _ ->
                        [?SPEC_SUPERVISOR(random_primes_filter_start_child)|
                         [?SPEC_SUPERVISOR(random_primes_filter_sup)|ChildSpecs2]]
                  end,

    {ok, {SupFlags, ChildSpecs3}}.
