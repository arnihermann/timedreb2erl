-module(monitor).
-export([init/1, stateChange/3, monitorType/0]).

-include("$moduleName$.hrl").
-include("\$MCERLANG_HOME/src/include/stackEntry.hrl").

-behaviour(mce_behav_monitor).

monitorType() -> safety.

init(State) -> {ok, State}.

stateChange(State, MonState, Stack) ->
  case mce_erl:has_probe_state(instance_label, State) of
    true ->
      ProcessState = mce_erl:get_probe_state(instance_label, State),
      if
        %ProcessState#label_statevars.failIfTrue -> {false, stateReason};
        true -> {ok, MonState}
      end;
    _Else -> {ok, MonState}
  end.

