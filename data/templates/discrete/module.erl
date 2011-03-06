-module($moduleName$).
-export([main/0]).

-include("$moduleName$.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/state.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/process.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/node.hrl").

% messages related macros
tr_sorted_messages(Bag, Now) ->
  NewBag = tr_new_messages(Bag),
  lists:filter(tr_prune(Now), lists:sort(fun tr_sort/2, NewBag)).

tr_new_messages(Bag) ->
  receive
    Msg -> tr_new_messages([Msg | Bag])
  after 0 ->
    Bag
  end.

tr_sort(E1, E2) ->
  {{_, TT1, _}, _, _} = E1,
  {{_, TT2, _}, _, _} = E2,
  TT1 < TT2.

tr_prune(CurrentTime) ->
  fun(E) ->
    {{_, _, DL}, _, _} = E,
    (DL == inf) orelse (not (CurrentTime > DL))
  end.

% normal messages
tr_send(Now, Rebec, Msg) ->
  tr_send(Now, Rebec, Msg, {}).

tr_send(Now, Rebec, Msg, Params) ->
  tr_send(Now, Rebec, Msg, Params, inf).

tr_send(Now, Rebec, Msg, Params, Deadline) ->
  Rebec ! {{self(), Now, Deadline}, Msg, Params}.

% messages with delay
tr_sendafter(After, Rebec, Msg) ->
  tr_sendafter(After, Rebec, Msg, {}).

tr_sendafter(After, Rebec, Msg, Params) ->
  tr_sendafter(After, Rebec, Msg, Params, inf).

tr_sendafter(After, Rebec, Msg, Params, Deadline) ->
  Rebec ! {{self(), After, Deadline}, Msg, Params}.

tr_nondet(Exp) ->
  mce_erl:choice(lists:map(fun(E) -> fun() -> E end end, Exp)).

$reactiveClasses; separator="

"$

main() ->
  $main$.
