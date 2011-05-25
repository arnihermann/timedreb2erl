-module(monitor).
-export([init/1, stateChange/3, monitorType/0]).

-include("$MCERLANG_HOME/src/include/stackEntry.hrl").

-behaviour(mce_behav_monitor).


monitorType() -> safety.

init(State) -> {ok, State}.

check(State, Instance, Pred) ->
  case mce_erl:has_probe_state(Instance, State) of
    true ->
      ProcessState = mce_erl:get_probe_state(Instance, State),
      Pred(ProcessState);
    false -> false
  end.

stateChange(State, MonState, Stack) ->
  %P1 = check(State, admin, fun(ProcessState) -> ProcessState#admin_statevars.scientistDead end),
  if
    %P1 -> {false, scientistDeadInAdmin};
    true -> {ok, MonState}
  end.


