-module(run).
-language(erlang).

-export([simulate/1]).

-include("\$MCERLANG_HOME/src/include/mce_opts.hrl").

simulate(Args) ->
  mce:start(#mce_opts{program={$moduleName$, main, Args},
                      monitor={monitor, []},
                      time_limit=1200,
                      %chatter=all,
                      algorithm={mce_alg_simulation, void}}).
