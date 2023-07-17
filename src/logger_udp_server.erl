-module(logger_udp_server).

-behaviour(gen_server).

-export([get_connections/1, send/3]).

-export([start/2]).

-export([init/1, handle_call/3, handle_cast/2]).

get_connections(Type) ->
    Count = erlang:system_info(schedulers),
    Pid = case start(Type, Count) of
              {ok, P} -> P;
              {error, {already_started, P}} -> P
          end,
    gen_server:call(Pid, socks).

send(Socket, Addr, Data) ->
    gen_udp:send(Socket, Addr, Data).

start(Type, Count) ->
    gen_server:start({local, procname(Type)}, ?MODULE, {Type, Count}, []).

init({Type, Count}) ->
    Socks = [{Id, start_sock(Type)} || Id <- lists:seq(0, Count - 1)],
    Map = maps:from_list(Socks),
    {ok, Map}.

start_sock(Type) ->
    {ok, Sock} = gen_udp:open(0, [Type, binary, {active, false}]),
    Sock.

handle_call(socks, _From, Socks) ->
    {reply, Socks, Socks}.

handle_cast(_Msg, Socks) ->
    {noreply, Socks}.

procname(inet) -> logger_udp_inet;
procname(inet6) -> logger_udp_inet6;
procname(local) -> logger_udp_local.
