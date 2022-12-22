%%%-------------------------------------------------------------------
%% @doc random_primes top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("random_primes.hrl").

-define(SERVER, ?MODULE).

-define(SPEC_WORKER(Id), #{id => Id, start => {Id, start_link, []}}).
-define(SPEC_WORKER(Id, Args), #{id => Id, start => {Id, start_link, Args}}).
-define(SPEC_WORKER(Id, M, F, Args), #{id => Id, start => {M, F, Args}}).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
    PrimRange = random_primes_lib:get_env(?APP, prime_range, ?PRIME_RANGE),
    RatePerSecond = random_primes_lib:get_env(?APP, rate_per_second, ?RATE_PER_SECOND),

    EredisHost = random_primes_lib:get_env(?EREDIS, host, ?LOCAL_HOST),
    EredisPort = random_primes_lib:get_env(?EREDIS, port, ?EREDIS_PORT),
    EredisDB = random_primes_lib:get_env(?EREDIS, database, ?EREDIS_DEFAULT_DB),

    ChildSpecs = [?SPEC_WORKER(eredis, [EredisHost, EredisPort, EredisDB])],
    ChildSpecs2 = case random_primes_lib:get_env(?APP, generator) of
                    true -> [?SPEC_WORKER(random_primes_gen, [PrimRange, RatePerSecond])|ChildSpecs];
                    _ -> ChildSpecs
                  end,
    ChildSpecs3 = case random_primes_lib:get_env(?APP, filter) of
                    true -> [?SPEC_WORKER(random_primes_filter, [PrimRange, RatePerSecond])|ChildSpecs2];
                    _ -> ChildSpecs2
                  end,

    {ok, {SupFlags, ChildSpecs3}}.
 
%% internal functions
