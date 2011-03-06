$reactiveClassName$(Env, InstanceName) ->
  receive
    {$knownRebecNames; separator=", "$} ->
      $reactiveClassName$(Env, InstanceName, $knownRebecRecordName${$knownRebecValues:{ k |$k.0$=$k.1$}; separator=", "$})
  end.
$reactiveClassName$(Env, InstanceName, KnownRebecs) ->
  StateVars = $if(stateVarRecordName)$$stateVarRecordName${}$else$undefined$endif$,
  LocalVars = $if(localVarRecordName)$$localVarRecordName${}$else$undefined$endif$,
  receive
$msgSrvInit$
  end.
$reactiveClassName$(Env, InstanceName, KnownRebecs, StateVars) ->
  LocalVars = $if(localVarRecordName)$$localVarRecordName${}$else$undefined$endif$,
  receive
$msgSrvs; separator=";
"$
  end.
