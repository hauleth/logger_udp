% SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(logger_udp_h_SUITE).

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
        logged_data_is_received,
        configuration_do_not_contain_sockets,
        change_config_to_unix_socket,
        formatter
    ].

format(#{msg := {string, Data}}, _Opts) ->
    Data;
format(#{msg := Other}, _Opts) ->
    io_lib:format("~p", [Other]).

init_per_group(Type, Config) when
    Type =:= inet;
    Type =:= inet6;
    Type =:= local
->
    Name = Type,
    {Pid, {Socket, LoggerConfig}} = start_proc(fun() -> upstream(Type) end),
    ct:pal("Logger config: ~p", [LoggerConfig]),
    logger:add_handler(Name, logger_udp_h, #{
        config => LoggerConfig,
        formatter => {?MODULE, []}
    }),
    [{socket, Socket}, {socket_pid, Pid}, {name, Name} | Config];
init_per_group(_Name, Config) ->
    Config.

end_per_group(Type, Config) when
    Type =:= inet;
    Type =:= inet6;
    Type =:= local
->
    Name = Type,
    SPid = ?config(socket_pid, Config),
    SPid ! stop,
    logger:remove_handler(Name),
    [
        catch gen_server:stop(PName)
     || PName <- [
            logger_udp_inet,
            logger_udp_inet6,
            logger_udp_local
        ]
    ],
    Config;
end_per_group(_Name, Config) ->
    Config.

init_per_testcase(_Name, Config) ->
    Name = ?config(name, Config),
    {ok, OldConfig} = logger:get_handler_config(Name),
    [{old_config, OldConfig} | Config].
end_per_testcase(_Name, Config) ->
    Name = ?config(name, Config),
    OldConfig = ?config(old_config, Config),
    logger:set_handler_config(Name, OldConfig),
    Config.

start_proc(Fn) ->
    Ref = make_ref(),
    Parent = self(),
    Pid = spawn(fun() ->
        Parent ! {Ref, Fn()},
        receive
            stop -> ok
        end
    end),
    receive
        {Ref, Data} -> {Pid, Data}
    after 1000 -> ct:fail("Cannot start socket")
    end.

upstream(local, Path) ->
    P = unicode:characters_to_binary(Path),
    %% We need to use `list` mode there because of
    %% https://github.com/erlang/otp/issues/7605
    {ok, Sock} = gen_udp:open(0, [
        {ip, {local, P}},
        local,
        list,
        {active, false}
    ]),
    Self = self(),
    % Clean on exit
    spawn(fun() ->
        Ref = monitor(process, Self),
        receive
            {'DOWN', Ref, process, Self, _} ->
                file:delete(P),
                ok
        end
    end),
    {Sock, #{path => P}}.

upstream(local) ->
    upstream(local, <<"socket">>);
upstream(inet) ->
    Host = {127, 0, 0, 1},
    {ok, Sock} = gen_udp:open(0, [
        inet,
        binary,
        {active, false}
    ]),
    {ok, Port} = inet:port(Sock),
    {Sock, #{host => Host, port => Port}};
upstream(inet6) ->
    Host = {0, 0, 0, 0, 0, 0, 0, 1},
    {ok, Sock} = gen_udp:open(0, [
        inet6,
        binary,
        {active, false}
    ]),
    {ok, Port} = inet:port(Sock),
    {Sock, #{host => Host, port => Port}}.

recv(Config) ->
    Socket = ?config(socket, Config),
    {ok, {_Addr, _Port, Data}} = gen_udp:recv(Socket, 0, 2000),
    {ok, iolist_to_binary(Data)}.

%% Tests

logged_data_is_received(Config) ->
    logger:notice("Foo"),
    {ok, Data} = recv(Config),
    ct:pal("~p", [Data]),
    ?assertEqual(<<"Foo">>, Data),
    ok.

configuration_do_not_contain_sockets(Config) ->
    Name = ?config(name, Config),
    {ok, HConfig} = logger:get_handler_config(Name),
    #{config := LConfig} = HConfig,
    ?assertNot(is_map_key(sockets, LConfig)).

formatter(Config) ->
    Name = ?config(name, Config),
    ok = logger:update_handler_config(Name, #{formatter => {sample_formatter, []}}),
    logger:notice("Foo"),
    {ok, Data} = recv(Config),
    ?assertEqual(<<"sample_formatter notice: Foo">>, Data),
    ok.

change_config_to_unix_socket(Config) ->
    Name = ?config(name, Config),
    {Sock, NewConfig} = upstream(local, "new_sock"),
    ok = logger:update_handler_config(Name, #{config => NewConfig}),
    logger:notice("Foo"),
    {ok, {_Addr, _Port, Data}} = gen_udp:recv(Sock, 0, 2000),
    ?assertEqual(<<"Foo">>, iolist_to_binary(Data)).
change_config_to_inet(_Config) -> ok.
change_config_to_inet6(_Config) -> ok.
