-module(propsd_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  {ok, {#{strategy => one_for_one}, [#{
    id => propsd_client,
    start => {propsd_client, start_link, []},
    restart => permanent
  }]}}.
