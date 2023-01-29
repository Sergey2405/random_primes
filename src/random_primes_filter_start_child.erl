%%%-------------------------------------------------------------------
%% @doc random_primes_filter_start_child
%% @end
%%%-------------------------------------------------------------------

-module(random_primes_filter_start_child).
-behaviour(gen_server).

-export([start_link/0]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).
-export([observer/1,
         make_childs/2]). 

-include("random_primes.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    % supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    MaxNumberOfProcesses = random_primes_lib:get_env(?APP, filter, max_processes, ?MAX_FILTER_PROCESSES),
    FilterType = random_primes_lib:get_env(?APP, filter, type, static),
    case FilterType of
        dynamic ->
            spawn_link(?MODULE, observer, [random_primes_lib:get_env(?APP, filter, dynamic_interval, ?FILTER_DYNAMIC_INTERVAL)]);
        _ ->
            timer:apply_after(1000, ?MODULE, make_childs, [add, MaxNumberOfProcesses])
    end,
    {ok, #state{}}.

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

observer(Delay) ->
    io:format("observer self()=~p~n",[self()]),
    put(llen_history, [0]),
    loop_observer(Delay).

loop_observer(Delay) ->

    NumberOfProcesses = length(supervisor:which_children(random_primes_filter_sup)),

    EredisProc = random_primes_lib:get_eredis_supervisioned_proc(),
    {ok, BinaryValue} =  eredis:q(EredisProc, ["LLEN", random_primes_lib:get_env(?EREDIS, number_list_key)]),

    CurrLlen = list_to_integer(binary_to_list(BinaryValue)),
    LHistory = get(llen_history),
    MaxLlen = lists:max(LHistory),
    NewLHistory= lists:sublist([CurrLlen|LHistory], 1, 3),
    put(llen_history, NewLHistory),

    % io:format("observer: ~p: Currllen=~p NumberOfProcesses=~p~n",
    %           [calendar:now_to_datetime(os:timestamp()),
    %            CurrLlen, NumberOfProcesses]),

    if  CurrLlen > MaxLlen ->
            make_childs(add, max(NumberOfProcesses div 2, 1));
        true ->
            ok
    end,
    timer:sleep(Delay),
    loop_observer(Delay).


make_childs(add, Number) when Number > 0 ->
    add_child(),
    make_childs(add, Number - 1);
make_childs(delete, Number) when Number > 0 ->
    delete_child(),
    make_childs(delete, Number - 1);
make_childs(_, _) -> ok.

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
    NumberOfProcesses = length(ProcIxs),
    if NumberOfProcesses > 1 ->
        ProcToKill = hd([Pid || {_, Pid, _, _} <- supervisor:which_children(random_primes_filter_sup)]),
        supervisor:terminate_child(random_primes_filter_sup, ProcToKill);
       true -> undefined
    end.

get_proc_ixs() -> 
    lists:sort([list_to_integer(atom_to_list(Name) --"filter_") || 
                {registered_name, Name} <- [(process_info(Pid, registered_name)) || 
                {_, Pid, _, _} <- supervisor:which_children(random_primes_filter_sup)]]).
