% SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%% @private

-module(logger_udp_server).

-behaviour(gen_server).

-export([get_connections/1, send/3]).

-export([start/2]).

-export([init/1, handle_call/3, handle_cast/2]).

-export_type([socket/0, sock_type/0]).

-type socket() :: term().
-type sock_type() :: inet | inet6 | unix.

%% @doc Return list of connections for given socket type.
-spec get_connections(atom()) -> [socket()].
get_connections(Type) ->
    % Schedulers can be set only at startup and cannot change during the system
    % life (you can only set schedulers_online to value not larger than this
    % one). So this should be secure.
    Count = erlang:system_info(schedulers),
    Pid =
        case start(Type, Count) of
            {ok, P} -> P;
            {error, {already_started, P}} -> P
        end,
    gen_server:call(Pid, socks).

-spec send(socket(), Addr :: term(), Data :: iodata()) -> ok.
send(Socket, Addr, Data) ->
    gen_udp:send(Socket, Addr, Data),
    ok.

start(Type, Count) ->
    gen_server:start({local, procname(Type)}, ?MODULE, {Type, Count}, []).

init({Type, Count}) ->
    Socks = [start_sock(Type) || _Id <- lists:seq(1, Count)],
    Tuple = list_to_tuple(Socks),
    {ok, Tuple}.

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
