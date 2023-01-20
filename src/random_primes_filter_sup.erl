%%%-------------------------------------------------------------------
%% @doc random_primes_filter_sup level supervisor. 
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_filter_sup).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-include("random_primes.hrl").

-define(SERVER, ?MODULE). 

-define(SPEC_WORKER(Id), #{id => Id, start => {Id, start_link, []}}).
-define(SPEC_WORKER(Id, Args), #{id => Id, start => {Id, start_link, Args}}). 
-define(SPEC_WORKER(Id, M, Args), #{id => Id, start => {M, start_link, Args}}).
-define(SPEC_WORKER(Id, M, F, Args), #{id => Id, start => {M, F, Args}}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    io:format("~p:init self() ~p~n",[?MODULE,self()]), 
    % process_flag(trap_exit, false), 
    % spawn_link(?MODULE, observer, [1000]), 

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1000, 
                 period => 10}, 
    PrimeRange = random_primes_lib:get_env(?APP, prime_range, ?PRIME_RANGE),
    RatePerSecond = random_primes_lib:get_env(?APP, rate_per_second, ?RATE_PER_SECOND),
    ChildSpecs = [?SPEC_WORKER(random_primes_filter, [PrimeRange, RatePerSecond])],

    {ok, {SupFlags, ChildSpecs}}.
