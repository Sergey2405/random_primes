-module(random_primes_gen).
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
-export([start_random_number_loop/1]).

-include("random_primes.hrl").

-record(state, {
    prime_range = ?PRIME_RANGE :: pos_integer(),
    rate_per_second = ?RATE_PER_SECOND :: pos_integer()}).

start_link(PrimeRange, RatePerSecond) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PrimeRange, RatePerSecond], []).

stop() ->
    gen_server:stop(?MODULE).

init([PrimeRange, RatePerSecond]) ->
    spawn_link(?MODULE, start_random_number_loop, [RatePerSecond]),
    {ok, #state{prime_range = PrimeRange,
                rate_per_second = RatePerSecond}}.

handle_call(generate_random_number_evenly, _From, State) ->
    RandomNumber = erlang:phash2(os:timestamp(), State#state.prime_range - 1) +2,
    EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
    eredis:q_async(EredisProc, ["LPUSH", random_primes_lib:get_env(?EREDIS, number_list_key), RandomNumber]),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    logger:error("Unexpected Request ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:info("Unexpected Msg ~p", [Msg]),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    logger:info("Terminate with reason", [Reason]),
    ok.

start_random_number_loop(Rate) ->
    put(ts_history, [os:timestamp()]),
    Delay = random_primes_lib:calc_delay(Rate),
    random_number_loop(Rate, Delay).

random_number_loop(Rate, Delay) ->
    CurrentTS = os:timestamp(),
    TSHistory = get(ts_history),
    OldTS = lists:last(TSHistory),
    NewTSHistory= lists:sublist([CurrentTS|TSHistory], 1, Rate),
    LastDelay = timer:now_diff(CurrentTS, OldTS),
    put(ts_history, NewTSHistory),

    gen_server:call(?MODULE, generate_random_number_evenly),

    if  LastDelay > 1000000 -> no_sleep; % us
        true -> timer:sleep(Delay) % ms
    end,

    random_number_loop(Rate, Delay).
