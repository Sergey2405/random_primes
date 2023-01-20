%%%-------------------------------------------------------------------
%% @doc random_primes_filter_start_child
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_filter_start_child).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-export([
         observer/1 
         ]). 

-include("random_primes.hrl").

-define(SERVER, ?MODULE).

-define(SPEC_WORKER(Id), #{id => Id, start => {Id, start_link, []}}). 
-define(SPEC_WORKER(Id, Args), #{id => Id, start => {Id, start_link, Args}}). 
-define(SPEC_WORKER(Id, M, Args), #{id => Id, start => {M, start_link, Args}}).
-define(SPEC_WORKER(Id, M, F, Args), #{id => Id, start => {M, F, Args}}). 

-define(SPEC_SUPERVISOR(Id), #{id => Id, 
                               start => {Id, start_link, []}, 
                               type => supervisor}). 

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    process_flag(trap_exit, false), 

    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
    PrimeRange = random_primes_lib:get_env(?APP, prime_range, ?PRIME_RANGE),
    RatePerSecond = random_primes_lib:get_env(?APP, rate_per_second, ?RATE_PER_SECOND),
 

    Link = spawn_link(?MODULE, observer, [0]), 
    io:format("spawn_link ~p~n",[Link]), 
    {ok, {SupFlags, []}}.

observer(Delay) ->
    io:format("observer self()~p~n",[self()]), 
    put(llen, 0),
    loop_observer(Delay).

loop_observer(Delay) ->
    EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
    NumberOfProcesses = length(supervisor:which_children(random_primes_filter_sup)),
    {ok, BinaryValue} =  eredis:q(EredisProc, ["LLEN", random_primes_lib:get_env(?EREDIS, number_list_key)]),
    CurrLlen = list_to_integer(binary_to_list(BinaryValue)),
    io:format("observe: ~p: llen=~p NumberOfProcesses=~p~n",
              [calendar:now_to_datetime(os:timestamp()),
               CurrLlen,NumberOfProcesses]),
    PrevLlen =  put(llen, CurrLlen),
    % io:format("observer<4.2>~n"), 
    NewDelay = 
        if  PrevLlen == 0 ->
                do_childs(delete, NumberOfProcesses div 4),
                Delay + Delay div 4;%ms
            CurrLlen > PrevLlen ->
                % io:format("observer<4.2.1>"), 
                do_childs(add, NumberOfProcesses + 1),
                Delay div 4 + 1000;%ms 
            true ->
                % io:format("observer<4.2.2>"), 
                do_childs(delete, NumberOfProcesses div 4),
                Delay + Delay div 4%ms
        end,
    % io:format("observer<5>~n"), 
    timer:sleep(Delay),
    loop_observer(NewDelay).


do_childs(add, Number) when Number > 0 -> 
    add_child(),
    do_childs(add, Number - 1);
do_childs(delete, Number) when Number > 0 -> 
    delete_child(),
    do_childs(delete, Number - 1);
do_childs(_, Number) -> ok.

add_child() ->
    ProcIxs = get_proc_ixs(),
    NumberOfProcesses = length(ProcIxs),
    NewIx = 
      case lists:seq(1, NumberOfProcesses) -- ProcIxs of
        [] -> NumberOfProcesses + 1;
        [H|_] -> H
      end,
    supervisor:start_child(random_primes_filter_sup, [list_to_atom("filter_" ++ integer_to_list(NewIx))]).

delete_child() ->
    ProcIxs = get_proc_ixs(),
    NumberOfProcess = length(ProcIxs),
    if NumberOfProcess > 1 ->
        ProcToKill = hd([Pid || {_, Pid, _, _} <- supervisor:which_children(random_primes_filter_sup)]),
        supervisor:terminate_child(random_primes_filter_sup, ProcToKill);
       true -> undefined
    end.

get_proc_ixs() -> 
    lists:sort([list_to_integer(atom_to_list(Name) --"filter_") || 
                {registered_name, Name} <- [(process_info(Pid, registered_name)) || 
                {_, Pid, _, _} <- supervisor:which_children(random_primes_filter_sup)]]).
