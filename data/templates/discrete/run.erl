-module($moduleName$_run).
-language(erlang).

-export([simulate/0]).

-include("\$MCERLANG_HOME/src/include/mce_opts.hrl").

simulate() ->
  mce:start(#mce_opts{program={$moduleName$, main, []},
      monitor={$moduleName$_monitor, []},
      time_limit=1200,
      %chatter=all,
      algorithm={mce_alg_simulation, void}}).
