        {{Sender, MsgTime, DL}, $msgSrvName$, {$formals; separator=", "$}} ->
          RebecTime = tr_rebec_time(Times),
          ProcTime = erlang:max(RebecTime, MsgTime),
          if
            (DL == inf) orelse (not (ProcTime > DL)) ->
              $statements$;
            true -> {RebecTime, StateVars, LocalVars}
          end
