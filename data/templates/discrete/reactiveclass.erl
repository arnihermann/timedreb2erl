$reactiveClassName$(InstanceName) ->
  receive
    {$knownRebecNames; separator=", "$} ->
      $reactiveClassName$(InstanceName, 0, [], $knownRebecRecordName${$knownRebecValues:{ k |$k.0$=$k.1$}; separator=", "$})
  end.
$reactiveClassName$(InstanceName, Time, Bag, KnownRebecs) ->
  StateVars = $if(stateVarRecordName)$$stateVarRecordName${}$else$undefined$endif$,
  LocalVars = $if(localVarRecordName)$$localVarRecordName${}$else$undefined$endif$,
  Bag0 = tr_sorted_messages(Bag, Time),
  if
    length(Bag0) > 0 ->
      [Msg | NewBag] = Bag0,
      {NewTime, NewStateVars, _} = case Msg of
$msgSrvInit$
      end,
      %mce_erl:probe_state(InstanceName, NewStateVars),
      mce_erl:probe_state(InstanceName, {NewTime, NewStateVars}),
      $reactiveClassName$(InstanceName, NewTime, NewBag, KnownRebecs, NewStateVars);
    true ->
      $reactiveClassName$(InstanceName, Time, Bag0, KnownRebecs, StateVars)
      %receive
        %Msg -> $reactiveClassName$(InstanceName, Time, [Msg | Bag0], KnownRebecs, StateVars)
      %end
  end.
$reactiveClassName$(InstanceName, Time, Bag, KnownRebecs, StateVars) ->
  LocalVars = $if(localVarRecordName)$$localVarRecordName${}$else$undefined$endif$,
  Bag0 = tr_sorted_messages(Bag, Time),
  if
    length(Bag0) > 0 ->
      [Msg | NewBag] = Bag0,
      {NewTime, NewStateVars, _} = case Msg of
$msgSrvs; separator=";
"$
      end,
      %mce_erl:probe_state(InstanceName, NewStateVars),
      mce_erl:probe_state(InstanceName, {NewTime, NewStateVars}),
      $reactiveClassName$(InstanceName, NewTime, NewBag, KnownRebecs, NewStateVars);
    true ->
      $reactiveClassName$(InstanceName, Time, Bag0, KnownRebecs, StateVars)
      %receive
        %Msg -> $reactiveClassName$(InstanceName, Time, [Msg | Bag0], KnownRebecs, StateVars)
      %end
  end.
