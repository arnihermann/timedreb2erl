-module($moduleName$).
-export([main/$length(envvars)$]).

-include("$moduleName$.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/state.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/process.hrl").
-include("\$MCERLANG_HOME/languages/erlang/src/include/node.hrl").

-define(RT_FACTOR, $rtFactor$).

% timed rebeca macros
tr_delay(T) ->
  receive
  after (T * ?RT_FACTOR) ->
    true
  end.

tr_timestamp({Mega, Sec, Micro}) ->
  (Mega * 1000000 + Sec) * 1000000 + Micro.

tr_now() ->
  tr_timestamp(erlang:now()).

%tr_deadline(AddSec) ->
  %{Mega, Sec, Micro} = erlang:now(),
  %(Mega * 1000000 + Sec + AddSec) * 1000000 + Micro.

% normal messages
tr_send(Rebec, Msg) ->
  tr_send(Rebec, Msg, {}).

tr_send(Rebec, Msg, Params) ->
  tr_send(Rebec, Msg, Params, inf).

tr_send(Rebec, Msg, Params, Deadline) ->
  Rebec ! {{self(), tr_now(), Deadline}, Msg, Params}.

% messages with delay
tr_sendafter(After, Rebec, Msg) ->
  tr_sendafter(After, Rebec, Msg, {}).

tr_sendafter(After, Rebec, Msg, Params) ->
  tr_sendafter(After, Rebec, Msg, Params, inf).

tr_sendafter(After, Rebec, Msg, Params, Deadline) ->
  Sender = self(),
  LocalNow = tr_now(),
  spawn(fun () ->
    receive
    after (After * ?RT_FACTOR) -> 
      Rebec ! {{Sender, LocalNow, Deadline}, Msg, Params}
    end
  end).

tr_nondet(Exp) ->
  lists:nth(random:uniform(length(Exp)), Exp).

tr_addTimeStamps(T1, T2) -> T1 + T2.

$reactiveClasses; separator="

"$

main($envvars; separator=", "$) ->
    $main$.
