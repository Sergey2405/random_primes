-module(random_primes_lib).

-export([get_env/2, get_env/3,
         get_eredis_supervisioned_proc/0,
         % get_supervisioned_proc/2,
         calc_delay/1,
         get_statistics/0,
         flushdb/0]).

-include("random_primes.hrl").

-spec get_env(atom(), atom()) ->'undefined' | {'ok', term()}.
get_env(Server, Application) ->
    case application:get_env(Server, Application) of
      {ok, Value} -> Value;
      _ -> undefined
    end.

-spec get_env(atom(), atom(), term()) -> term().
get_env(Server, Application, DefaultValue) ->
    application:get_env(Server, Application, DefaultValue).

-spec get_eredis_supervisioned_proc() -> pid() | undefined.
get_eredis_supervisioned_proc() ->
    get_supervisioned_proc(?SUPER_VISOR, ?EREDIS).

-spec get_supervisioned_proc(atom(), atom()) -> pid() | undefined.
get_supervisioned_proc(SuperVisor, Application) ->
    case [Pid || {App, Pid, _, _} <- supervisor:which_children(SuperVisor), App == Application] of
      [Proc] when is_pid(Proc)  -> Proc;
      _ -> undefined
    end.

-spec calc_delay(pos_integer()) -> pos_integer().
calc_delay(Rate) ->
    case 1000 div Rate of
      0 -> 10; % ms
      Value -> Value %ms
    end.

-spec get_statistics() -> #{number_of_primes => pos_integer(),
                            queue_length_of_random_numbers => pos_integer()}.
get_statistics() ->
    %% show only general statisctics - length of sizes and sets.
    EredisProc = get_eredis_supervisioned_proc(),

    {_, Scard} = eredis:q(EredisProc, ["SCARD", random_primes_lib:get_env(?EREDIS, prime_set_key)]),
    {_, LLEN} = eredis:q(EredisProc, ["LLEN", random_primes_lib:get_env(?EREDIS, number_list_key)]),

    Statisctics = maps:put(number_of_primes, Scard, #{}),
    maps:put(queue_length_of_random_numbers, LLEN, Statisctics).

-spec flushdb() ->
    {ok, any()} | {error, Reason::binary() | no_connection}.
flushdb() ->
    EredisProc = get_eredis_supervisioned_proc(),
    eredis:q(EredisProc, ["FLUSHDB"]).
