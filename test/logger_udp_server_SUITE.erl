% SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(logger_udp_server_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        {group, inet},
        {group, inet6},
        {group, local}
    ].

groups() ->
    [
        {inet, [], tests()},
        {inet6, [], tests()},
        {local, [], tests()}
    ].

tests() ->
    [
        contains_N_sockets,
        contained_map_has_unique_values
    ].

init_per_group(Type, Opts) when
    Type =:= inet;
    Type =:= inet6;
    Type =:= local
->
    [{type, Type} | Opts];
init_per_group(_Name, Opts) ->
    Opts.

end_per_group(_, Opts) -> Opts.

init_per_testcase(_Name, Opts) -> Opts.

end_per_testcase(_Name, Opts) ->
    [
        catch gen_server:stop(Name)
     || Name <- [
            logger_udp_inet,
            logger_udp_inet6,
            logger_udp_local
        ]
    ],
    Opts.

contains_N_sockets(Opts) ->
    Type = ?config(type, Opts),
    {ok, Pid} = logger_udp_server:start(Type, 10),
    Sockets = gen_server:call(Pid, socks),
    ?assertEqual(10, tuple_size(Sockets)).

contained_map_has_unique_values(Opts) ->
    Type = ?config(type, Opts),
    {ok, Pid} = logger_udp_server:start(Type, 10),
    SocketsMap = gen_server:call(Pid, socks),
    Sockets = tuple_to_list(SocketsMap),
    ?assertEqual(Sockets, lists:uniq(Sockets)).
