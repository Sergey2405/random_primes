-module(random_primes_filter).
-behaviour(gen_server).

%% API
-export([start_link/2,
         stop/0]).
%% gen_server callbacks
-export([init/1,
	     handle_call/3,
	     handle_cast/2,
	     handle_info/2,
         code_change/3,
         terminate/2]).
-export([start_filter_loop/1,
         create_prime_list/1]).
%% Tests
-export([checks_primes_in_db/0]).

-include("random_primes.hrl").

-record(state, {
    prime_range = ?PRIME_RANGE :: pos_integer(),
    rate_per_second = ?RATE_PER_SECOND :: pos_integer(),
    prime_list = [2]:: [pos_integer()],
    filter_loop_proc :: pid()}).

start_link(PrimeRange, RatePerSecond) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PrimeRange, RatePerSecond], []).

stop() ->
    gen_server:stop(?MODULE).

init([PrimeRange, RatePerSecond]) ->
    spawn(?MODULE, create_prime_list, [PrimeRange]),
    {ok, #state{prime_range = PrimeRange,
                rate_per_second = RatePerSecond,
                filter_loop_proc =  spawn_link(?MODULE, start_filter_loop, [RatePerSecond])}}.

handle_call(Request, _From, State) ->
    logger:error("Unexpected Request ~p", [Request]),
    {reply, ok, State}.

handle_cast({is_in_prime_list, BinaryValue}, State) ->
    try list_to_integer(binary_to_list(BinaryValue)) of
      Number ->
        EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
        case lists:member(Number, State#state.prime_list) of
          true ->
            eredis:q_async(EredisProc, ["SADD", random_primes_lib:get_env(?EREDIS, prime_set_key), Number]);
          false ->
            ok
        end
    catch
        _:_ -> 0
    end,
    {noreply, State};
handle_cast({set_prime_list, PrimeList}, State) ->
    logger:info("Init new range of prime list"),
    LoopProc = State#state.filter_loop_proc,
    LoopProc ! start_filter_loop,
    {noreply, State#state{prime_list = PrimeList}};
handle_cast(Msg, State) ->
    logger:info("Unexpected Msg ~p", [Msg]),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    logger:info("Terminate with reason", [Reason]),
    ok.

start_filter_loop(Rate) ->
    receive start_filter_loop -> continue
    end,
    Delay = random_primes_lib:calc_delay(Rate),
    filter_loop(Delay).

filter_loop(Delay) ->
    EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
    case eredis:q(EredisProc, ["RPOP", random_primes_lib:get_env(?EREDIS, number_list_key)]) of
      {ok, Value} when Value =/= undefined ->
        gen_server:cast(?MODULE, {is_in_prime_list, Value});
      _Any -> timer:sleep(Delay)
    end,
    filter_loop(Delay).

-spec create_prime_list(pos_integer()) -> [pos_integer()].
create_prime_list(Int) ->
    InitPrimeList = [3, 2],
    PrimeList = create_prime_list(Int, InitPrimeList),
    gen_server:cast(?MODULE, {set_prime_list, PrimeList}),
    PrimeList.

-spec create_prime_list(pos_integer(), [pos_integer()]) -> [pos_integer()].
create_prime_list(Int, PrimeList) ->
    [H|_T] = UpdatedPrimeList = next_prime_list(PrimeList),
    if 
      H < Int -> create_prime_list(Int, UpdatedPrimeList);
      true -> PrimeList
    end.

-spec next_prime_list([pos_integer()]) -> [pos_integer()].
next_prime_list(PrimeList = [H|_T]) ->
    next_prime_list(H + 2, PrimeList).

-spec next_prime_list(pos_integer(), [pos_integer()]) -> [pos_integer()].
next_prime_list(MayBePrime, PrimeList) ->
    case is_prime(MayBePrime) of
      true -> [MayBePrime|PrimeList];
      false -> next_prime_list(MayBePrime + 2, PrimeList)
    end.

-spec is_prime(pos_integer()) -> boolean().
is_prime(N) when is_integer(N),
                 N >= 2  ->
    is_prime(N, 2, erlang:trunc(math:sqrt(N)) + 1);
is_prime(_) -> false.

-spec is_prime(pos_integer(), pos_integer(), pos_integer()) -> boolean().
is_prime(_, Max, Max) ->
    true;
is_prime(N, Div, Max) ->
    if
      N rem Div =:= 0 -> false;
      true -> is_prime(N, Div + 1, Max)
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-spec checks_primes_in_db() -> ok | {{not_primes, [binary()]}, {convertation_errors, [binary()]}}.
checks_primes_in_db() ->
    EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
    case eredis:q(EredisProc, ["SMEMBERS", random_primes_lib:get_env(?EREDIS, prime_set_key)]) of
      {ok, Smemsers} -> do_checks_primes_in_db(Smemsers, EredisProc, [], []);
      Error -> Error
    end.

-spec do_checks_primes_in_db([binary()], pid()|undefined,
                             [binary()], [binary()]) ->
    ok | {{not_primes, binary()}, {convertation_errors, any()}}.
do_checks_primes_in_db([BinaryValue|BinaryValues], EredisProc,
                        NotPrimes, Errors) ->
    try list_to_integer(binary_to_list(BinaryValue)) of
      Number ->
        case is_prime(Number) of % integer and >= 2
          true ->
            do_checks_primes_in_db(BinaryValues, EredisProc,
                                   NotPrimes, Errors);
          false ->
            do_checks_primes_in_db(BinaryValues, EredisProc, 
                                   [BinaryValue|NotPrimes], Errors)
         end
    catch
      _:_ ->
        do_checks_primes_in_db(BinaryValues, EredisProc, 
                               NotPrimes, [BinaryValue|Errors])
    end;
do_checks_primes_in_db([], _, [], []) -> ok;
do_checks_primes_in_db(_, _, NotPrimes, Errors) ->
    {{not_primes, NotPrimes}, {convertation_errors, Errors}}.


%%%===================================================================
%%% Unit Tests
%%%===================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

prime_list_test_() ->
    PrimeListToTest = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97],
    CreatedPrimeList = create_prime_list(100),
    [?_assertEqual(lists:reverse(CreatedPrimeList), PrimeListToTest),
     ?_assertNot(is_prime(lists:nth(rand:uniform(length(PrimeListToTest)), PrimeListToTest) *
                          lists:nth(rand:uniform(length(PrimeListToTest)), PrimeListToTest))),
     ?_assert(is_prime(lists:nth(rand:uniform(length(PrimeListToTest)), PrimeListToTest)))].

-endif.
