-module(propsd_client).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start_link() ->
  {ok, pid()} | {error, already_started, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([fun()]) -> {ok, {[fun()], map()}}.
init([]) ->
  case application:get_env(propsd, url) of
    undefined -> ignore;
    {ok, URL} ->
      case hackney:connect(URL) of
        {ok, ConnRef} ->
          pull_props(ConnRef),
          {ok, ConnRef, 10000};
        _ -> {ok, retry, 1000}
      end
  end.

-spec handle_call(term(), term(), map()) -> {reply, term(), map()}.
handle_call(_Reqest, _From, State) ->
  {reply, {error, unexpected_call}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Reqest,  State) ->
  {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(timeout, retry) ->
  {ok, URL} = application:get_env(propsd, url),
  {ok, ConnRef} = hackney:connect(URL),
  pull_props(ConnRef),
  {ok, ConnRef, 10000};
handle_info(_Info,  State) ->
  {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_Old, State, _Extra) ->
  {ok, State}.

pull_props(ConnRef) ->
  {ok, _, _, ConnRef} = hackney:send_request(ConnRef, {get, <<"/v1/properties">>, [], <<>>}),
  {ok, Body} = hackney:body(ConnRef),
  Props = jsx:decode(Body, [return_maps]),
  maps:fold(fun
    (<<"erlang.", Key/binary>>, Value, _Acc) ->
      [Application, Var | Rest] = lists:map(fun try_to_atom/1, binary:split(Key, <<".">>, [global])),
      deep_set_env(Application, Var, Rest, Value);
    (_, _, _Acc) -> ok
  end, [], Props).

deep_set_env(Application, Var, Rest, Value) ->
  Var_Env = application:get_env(Application, Var, #{}),
  application:set_env(Application, Var, init_or_merge(Var_Env, Rest, Value)).

init_or_merge(_Acc, [], Value) ->
  try_to_atom(Value);
init_or_merge(Acc, [Key | Rest], Value) when is_map(Acc) ->
  maps:merge(Acc, #{Key => init_or_merge(maps:get(Key, Acc, #{}), Rest, Value)});
init_or_merge([Head | _Rest] = Acc, [Key | Rest], Value) when not is_integer(Head) ->
  [{Key, init_or_merge(proplists:get_value(Key, Acc, []), Rest, Value)} | Acc];
init_or_merge([], [Key | Rest], Value) ->
  [{Key, init_or_merge([], Rest, Value)}];
init_or_merge(_Acc, [Key | Rest], Value) ->
  #{Key => init_or_merge(#{}, Rest, Value)}.

try_to_atom(K) ->
  try binary_to_existing_atom(K, utf8)
  catch
    _Class:_Err ->
      K
  end.

-ifdef(TEST).
-compile(export_all).

pull_props_test() ->
  Conn = conn,
  meck:new(hackney),
  meck:expect(hackney, send_request, fun(conn, {get, <<"/v1/properties">>, [], <<>>}) -> {ok, [], [], conn} end),
  meck:expect(hackney, body, fun(conn) -> {ok, <<"{\"erlang.propsd.a\": \"a\"}">>} end),
  [] = application:get_all_env(propsd),
  pull_props(Conn),
  [{a, a}] = application:get_all_env(propsd),
  meck:unload(hackney).

init_or_merge_test() ->
  #{a := a} = init_or_merge(#{}, [a], a),
  [{a, a}] = init_or_merge([], [a], a).

try_to_atom_test() ->
  a = try_to_atom(<<"a">>),
  <<"not an atom">> = try_to_atom(<<"not an atom">>).

-endif.
