%%%-------------------------------------------------------------------
%% @doc random_primes_filter_start_child
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_filter_start_child).
-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-export([observer/1,
         make_childs/2]). 

-include("random_primes.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    process_flag(trap_exit, false), 

    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
 
    MaxNumberOfProcesses = random_primes_lib:get_env(?APP, filter, max_processes, ?MAX_FILTER_PROCESSES),
    FilterType = random_primes_lib:get_env(?APP, filter, type, static),
    case FilterType of
        dynamic ->
            spawn_link(?MODULE, observer, [1000]);
        _ ->
            timer:apply_after(1000, ?MODULE, make_childs, [add, MaxNumberOfProcesses])
    end,

    {ok, {SupFlags, []}}.

observer(Delay) ->
    put(llen, 0),
    loop_observer(Delay).

loop_observer(Delay) ->

    MinInterval = random_primes_lib:get_env(?APP, filter, min_dynamic_interval, ?MIN_FILTER_DYNAMIC_INTERVAL),
    MaxInterval = random_primes_lib:get_env(?APP, filter, max_dynamic_interval, ?MAX_FILTER_DYNAMIC_INTERVAL),

    EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
    NumberOfProcesses = length(supervisor:which_children(random_primes_filter_sup)),
    {ok, BinaryValue} =  eredis:q(EredisProc, ["LLEN", random_primes_lib:get_env(?EREDIS, number_list_key)]),

    CurrLlen = list_to_integer(binary_to_list(BinaryValue)),
    PrevLlen =  put(llen, CurrLlen),

    % io:format("observer: ~p: Currllen=~p NumberOfProcesses=~p~n",
    %           [calendar:now_to_datetime(os:timestamp()),
    %            CurrLlen,NumberOfProcesses]),

    NewDelay = 
        if  PrevLlen < 1000 -> % magic number
                make_childs(delete, max(NumberOfProcesses div 4, 1)),
                min(Delay + Delay div 2, MaxInterval);%ms
            CurrLlen >= PrevLlen ->
                make_childs(add, max(NumberOfProcesses, 1)),
                max(Delay div 2, MinInterval);%ms
            true ->
                make_childs(delete, max(NumberOfProcesses div 4, 1)),
                min(Delay + Delay div 2, MaxInterval)%ms
        end,
    timer:sleep(Delay),
    loop_observer(NewDelay).


make_childs(add, Number) when Number > 0 ->
    add_child(),
    make_childs(add, Number - 1);
make_childs(delete, Number) when Number > 0 ->
    delete_child(),
    make_childs(delete, Number - 1);
make_childs(_, Number) -> ok.

add_child() ->
    ProcIxs = get_proc_ixs(),
    NumberOfProcesses = length(ProcIxs),
    NewIx = 
        case lists:seq(1, NumberOfProcesses) -- ProcIxs of
            [] -> NumberOfProcesses + 1;
            [H|_] -> H
        end,

    MaxNumberOfProcesses = random_primes_lib:get_env(?APP, filter, max_processes, ?MAX_FILTER_PROCESSES),
    if NumberOfProcesses < MaxNumberOfProcesses ->
        supervisor:start_child(random_primes_filter_sup, [list_to_atom("filter_" ++ integer_to_list(NewIx))]);
        true -> not_started
    end.

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
