-module($moduleName$).
-export([main/0]).

-include("$moduleName$.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/state.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/process.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/node.hrl").

% messages related macros
tr_sorted_messages(Bag, Times) ->
  lists:filter(tr_prune(Times), lists:sort(fun tr_sort/2, Bag)).

tr_sort(E1, E2) ->
  {_, {{_, TT1, _}, _, _}} = E1,
  {_, {{_, TT2, _}, _, _}} = E2,
  TT1 < TT2.

tr_prune(Times) ->
  fun(E) ->
    {To, {{_, _, DL}, _, _}} = E,
    case ets:lookup(Times, To) of
      [[{_, CurrentTime}]] -> (DL == inf) orelse (not (CurrentTime > DL));
      _ -> true
    end
  end.

% normal messages
tr_send(Now, Rebec, Msg) ->
  tr_send(Now, Rebec, Msg, {}).

tr_send(Now, Rebec, Msg, Params) ->
  tr_send(Now, Rebec, Msg, Params, inf).

tr_send(Now, Rebec, Msg, Params, Deadline) ->
  spirit ! {Rebec, {{self(), Now, Deadline}, Msg, Params}}.

% messages with delay
tr_sendafter(After, Rebec, Msg) ->
  tr_sendafter(After, Rebec, Msg, {}).

tr_sendafter(After, Rebec, Msg, Params) ->
  tr_sendafter(After, Rebec, Msg, Params, inf).

tr_sendafter(After, Rebec, Msg, Params, Deadline) ->
  spirit ! {Rebec, {{self(), After, Deadline}, Msg, Params}}.

tr_nondet(Exp) ->
  mce_erl:choice(lists:map(fun(E) -> fun() -> E end end, Exp)).

tr_rebec_time(Times) ->
  tr_rebec_time(Times, self()).

tr_rebec_time(Times, Rebec) ->
  case ets:lookup(Times, Rebec) of
    [{_, TimeTag}] -> TimeTag;
    _ -> 0
  end.


% spirit
%spirit(Times) ->
  %spirit([], Times).
%spirit(Bag, Times) ->
  %receive
    %Msg ->
      %spirit(tr_sorted_messages([Msg | Bag], Times), Times)
  %after 0 ->
    %case length(Bag) > 0 of
      %false ->
        %receive
          %Msg ->
            %spirit([Msg], Times)
        %end;
      %true ->
        %[{_, {{_, TT, _}, _, _}} | _] = Bag,
        %Messages = lists:takewhile(fun({_, {{_, TT0, _}, _, _}} ) -> TT =:= TT0 end, Bag),

        %Msg = tr_nondet(Messages),
        %{To, PayLoad} = Msg,
        %To ! PayLoad,
        %spirit(lists:delete(Msg, Bag), Times)
    %end
  %end.
%spirit(Times) ->
  %spirit([], Times).
%spirit(UnsortedBag, Times) ->
  %receive
    %Msg ->
      %spirit([Msg | UnsortedBag], Times)
  %after 0 ->
    %Bag = tr_sorted_messages(UnsortedBag, Times),
    %case length(Bag) > 0 of
      %false ->
        %receive
          %Msg ->
            %spirit([Msg], Times)
        %end;
      %true ->
        %%io:format("------ Bag status begins ~n"),
        %%lists:foreach(fun(E) -> io:format("   ~w~n", [E]) end, Bag),
        %%io:format("------ Bag status ends ~n"),
        %[{_, {{_, TT, _}, _, _}} | _] = Bag,
        %Messages = lists:takewhile(fun({_, {{_, TT0, _}, _, _}} ) -> TT =:= TT0 end, Bag),
        %Msg = tr_nondet(Messages),

        %{To, PayLoad} = Msg,
        %To ! PayLoad,
        %spirit(lists:delete(Msg, Bag), Times)
    %end
  %end.
spirit(Times) ->
  spirit([], Times).
spirit(UnsortedBag, Times) ->
  receive
    Msg ->
      spirit([Msg | UnsortedBag], Times)
  after 0 ->
    %io:format("****** UnsortedBag status begins ~n"),
    %lists:foreach(fun(E) -> io:format("   ~w~n", [E]) end, UnsortedBag),
    %io:format("****** UnsortedBag status ends ~n"),
    Bag = tr_sorted_messages(UnsortedBag, Times),
    case length(Bag) > 0 of
      false ->
        receive
          Msg ->
            spirit([Msg], Times)
        end;
      true ->
        %io:format("------ Bag status begins ~n"),
        %lists:foreach(fun(E) -> io:format("   ~w~n", [E]) end, Bag),
        %io:format("------ Bag status ends ~n"),
        [{_, {{_, TT, _}, _, _}} | _] = Bag,
        Messages = lists:takewhile(fun({_, {{_, TT0, _}, _, _}} ) -> TT =:= TT0 end, Bag),
        Msg = tr_nondet(Messages),

        {To, {{Sender, Time, Deadline}, MsgSrv, Formals}} = Msg,
        TimeTag = case ets:lookup(Times, To) of
          [{_, OldTime}] -> erlang:max(OldTime, Time);
          _ -> Time
        end,
        if
          not (TimeTag > Deadline) -> To ! {{Sender, TimeTag, Deadline}, MsgSrv, Formals};
          true -> ok
        end,
        spirit(lists:delete(Msg, Bag), Times)
    end
  end.


% reactive classes
$reactiveClasses; separator="

"$

main() ->
  $main$.
