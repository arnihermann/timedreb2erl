-module(rebeca).

delay(T) ->
  receive
  after (T * ?RT_FACTOR) ->
    true
  end.

timestamp({Mega, Sec, Micro}) ->
  (Mega * 1000000 + Sec) * 1000000 + Micro.

now() ->
  timestamp(erlang:now()).

% normal messages
send(Rebec, Msg) ->
  send(Rebec, Msg, {}).

send(Rebec, Msg, Params) ->
  send(Rebec, Msg, Params, inf).

send(Rebec, Msg, Params, Deadline) ->
  Rebec ! {{self(), now(), Deadline}, Msg, Params}.

% messages with delay
sendafter(After, Rebec, Msg) ->
  sendafter(After, Rebec, Msg, {}).

sendafter(After, Rebec, Msg, Params) ->
  sendafter(After, Rebec, Msg, Params, inf).

sendafter(After, Rebec, Msg, Params, Deadline) ->
  Sender = self(),
  LocalNow = now(),
  spawn(fun () ->
    receive
    after (After * ?RT_FACTOR) -> 
      Rebec ! {{Sender, LocalNow, Deadline}, Msg, Params}
    end
  end).

nondet(Exp) ->
  lists:nth(random:uniform(length(Exp)), Exp).

addTimeStamps(T1, T2) -> T1 + T2.

