%%%-------------------------------------------------------------------
%% @doc random_primes_filter_sup supervisor for filter(s).
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_filter_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-include("random_primes.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 1},

    RatePerSecond = random_primes_lib:get_env(?APP, generator, rate_per_second, ?RATE_PER_SECOND),

    ChildSpecs = [?SPEC_WORKER(random_primes_filter, [RatePerSecond])],

    {ok, {SupFlags, ChildSpecs}}.
