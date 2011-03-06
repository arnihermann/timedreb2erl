$reactiveClassName$(Times, InstanceName) ->
  receive
    {$knownRebecNames; separator=", "$} ->
      $reactiveClassName$(Times, InstanceName, $knownRebecRecordName${$knownRebecValues:{ k |$k.0$=$k.1$}; separator=", "$})
  end.
$reactiveClassName$(Times, InstanceName, KnownRebecs) ->
  StateVars = $if(stateVarRecordName)$$stateVarRecordName${}$else$undefined$endif$,
  LocalVars = $if(localVarRecordName)$$localVarRecordName${}$else$undefined$endif$,
  {NewTime, NewStateVars, _} = receive
$msgSrvInit$
  end,
  ets:insert(Times, {self(), NewTime}),
  mce_erl:probe_state(InstanceName, NewStateVars),
  $reactiveClassName$(Times, InstanceName, KnownRebecs, NewStateVars).
$reactiveClassName$(Times, InstanceName, KnownRebecs, StateVars) ->
  LocalVars = $if(localVarRecordName)$$localVarRecordName${}$else$undefined$endif$,
  {NewTime, NewStateVars, _} = receive
$msgSrvs; separator=";
"$
  end,
  ets:insert(Times, {self(), NewTime}),
  mce_erl:probe_state(InstanceName, NewStateVars),
  $reactiveClassName$(Times, InstanceName, KnownRebecs, NewStateVars).
