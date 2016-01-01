-module(shotgun).
-export([start/4, random_dna/1, approx_shortest_superstring/1, clean/1, sorted_by_length/1, devide/3, receive_seg/1]).

is_substring(Source, Needle) ->
    Pos = string:str(Source, Needle),
    if
        Pos > 0 ->
            true;
        true ->
            false
    end.

is_any_substring([] , _Needle) ->
	false;
is_any_substring([H | List], Needle) ->
	is_substring(H, Needle) orelse is_any_substring(List, Needle).

try_overlap(S1, S2, L) ->
	L1 = length(S1),
	L2 = length(S2),
	case L1 >= L andalso L2 >= L of
		false ->
			L - 1;
		true ->
			case string:substr(S1, L1 - L + 1) =:= string:substr(S2, 1, L) of
				true ->
					try_overlap(S1, S2, L + 1);
				false ->
					L - 1
			end
	end.

overlap(S1, S2) ->
	try_overlap(S1, S2, 0).

clean([]) ->
	[];
clean([String | StringList]) ->
	case String =:= "" orelse is_any_substring(StringList, String) of
		true -> 
			clean(StringList);
		false ->
			[String | clean(StringList)]
	end.

sorted_by_length(L) ->
	lists:sort(fun (A, B) -> length(A) < length(B) end, L).

% must be called with clean list, e.g. for all A, B. A is not a substring of b
% CleanList = clean(sorted_by_length(StringList))
approx_shortest_superstring([ S | [] ]) ->
	[S];
approx_shortest_superstring(StringList) ->
	StringSet = sets:from_list(StringList),
	AllPairs = lists:filter(
		fun({X, Y}) -> X =/= Y end,
		lists:flatmap(
			fun (X) -> lists:map(fun(Y) -> {X, Y} end,  StringList) end,
		StringList)
	),
	PairsWithOverlap = lists:map(fun({X, Y}) -> {overlap(X, Y), X, Y} end , AllPairs),
	{L , S1, S2} = lists:max(PairsWithOverlap),
	NewString = string:substr(S1 , 1 , length(S1) - L + 1) ++ string:substr(S2 , 1 + L),
	approx_shortest_superstring(sorted_by_length(sets:to_list(sets:subtract(StringSet, sets:from_list([S1 | [S2]]))) ++ [NewString])).

devide("" , _, List) ->
	List;
devide(S, Len, List) ->
	Length = round( ( rand:uniform() + 0.5 ) * Len),
	case length(S) =< Length of
		true ->
			[S | List];
		false ->
			{String , Rest} = lists:split(Length, S),
			devide(Rest, Len , [String | List])
	end.
start_merge([]) ->
	io:format("merging: nothing to merge.~n"),
	host!{result, [""]};
start_merge(L) ->
	io:format("merging: ~p~n", [L]),
	host!{result, approx_shortest_superstring(clean(sorted_by_length(L)))}.

receive_seg(L) ->
	receive
		{finished} ->
			start_merge(L);
		{seg, Segment} ->
			receive_seg([Segment | L])
	end.

spawn_processes(Id, Cores) when Id >= Cores ->
	[];
spawn_processes(Id, Cores) ->
	Pid = spawn_link(?MODULE , receive_seg , [[]]),
	[Pid | spawn_processes(Id + 1, Cores)].

random_dna(0) ->
	"";
random_dna(N) ->
	[lists:nth(rand:uniform(4), "ACTG") | random_dna(N - 1)].

create_segments(I, K, _L, _S) when I >= K ->
	[];
create_segments(I, K, L, S) ->
	devide(S, L , []) ++ create_segments(I + 1, K, L ,S).

send_to_process([] , _PList) ->
	ok;
send_to_process([H | T] , PList) ->
	lists:nth(rand:uniform(length(PList)), PList) ! {seg, H},
	send_to_process(T, PList).

send_end([]) ->
	ok;
send_end([PH | PT]) ->
	PH ! {finished},
	send_end(PT).

collect_results(I , Cores, L) when I >= Cores ->
	io:format("final result: ~s~n" , approx_shortest_superstring(clean(sorted_by_length(L))));
collect_results(I, Cores , L) ->
	receive
		{result, [S]} ->
			io:format("partial result: ~s~n" , [S]),
			collect_results(I + 1, Cores, [S | L])
	end.

start(N, K, L, Cores) ->
	register(host, self()),
	PList = spawn_processes(0, Cores),
	Seq = random_dna(N),
	io:format("DNA: ~s~n" , [Seq]),
	SegList = create_segments(0, K, L , Seq),
	send_to_process(SegList, PList),
	send_end(PList),
	collect_results(0 , Cores, []),
	unregister(host),
	ok.
