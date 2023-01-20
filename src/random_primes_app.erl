%%%-------------------------------------------------------------------
%% @doc random_prime public API
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    random_primes_sup:start_link().

stop(_State) ->
    ok.
