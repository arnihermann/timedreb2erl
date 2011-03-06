    {{Sender, TT, DL}, $msgSrvName$, {$formals; separator=", "$}} ->
      TimeNow = tr_now(),
      if
        ((TimeNow >= TT) andalso (DL == inf orelse TimeNow < DL)) ->
          {_, {NH, NM, NS}} = calendar:gregorian_seconds_to_datetime(TimeNow div 1000000),
          {_, {MH, MM, MS}} = calendar:gregorian_seconds_to_datetime(TT div 1000000),
          io:format("~s.$msgSrvName$ @~p:~p:~p by message @~p:~p:~p~n", [InstanceName, NH, NM, NS, MH, MM, MS]),
          %io:format("~s.$msgSrvName$~n", [InstanceName]),
          {NewStateVars, _} = $statements$,
          mce_erl:probe_state(InstanceName, NewStateVars),
          $reactiveClassName$(Env, InstanceName, KnownRebecs, NewStateVars);
        true ->
          {_, {NH, NM, NS}} = calendar:gregorian_seconds_to_datetime(TimeNow div 1000000),
          {_, {MH, MM, MS}} = calendar:gregorian_seconds_to_datetime(TT div 1000000),
          io:format("dropping ~s.$msgSrvName$ @~p:~p:~p by message @~p:~p:~p~n", [InstanceName, NH, NM, NS, MH, MM, MS]),
          %io:format("dropping ~s.$msgSrvName$~n", [InstanceName]),
          $reactiveClassName$(Env, InstanceName, KnownRebecs, StateVars)
      end
