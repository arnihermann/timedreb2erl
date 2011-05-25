-module(rebeca).

-export([delay/1]).
-export([now/0]).
-export([send/2, send/3, send/4]).
-export([sendafter/3, sendafter/4, sendafter/5]).
-export([nondet/1]).

-define(RT_FACTOR, 1000).

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
  Rebec ! {{self(), rebeca:now(), Deadline}, Msg, Params}.

% messages with delay
sendafter(After, Rebec, Msg) ->
  sendafter(After, Rebec, Msg, {}).

sendafter(After, Rebec, Msg, Params) ->
  sendafter(After, Rebec, Msg, Params, inf).

sendafter(After, Rebec, Msg, Params, Deadline) ->
  Sender = self(),
  LocalNow = rebeca:now(),
  spawn(fun () ->
    receive
    after (After * ?RT_FACTOR) -> 
      Rebec ! {{Sender, LocalNow, Deadline}, Msg, Params}
    end
  end).

nondet(Exp) ->
  lists:nth(random:uniform(length(Exp)), Exp).

addTimeStamps(T1, T2) -> T1 + T2.

