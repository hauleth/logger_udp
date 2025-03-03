% SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%% @doc Implementation of `logger' handler for UDP sinks.
%%
%% == Options ==
%%
%% <ul>
%%      <li>`host' - tuple specifying IPv4 or IPv6 address of the node where
%%      logs should be sent. If this field is set, then `port' field need to be
%%      set as werll.</li>
%%      <li>`port' - port which is used for receiving messages.</li>
%%      <li>`path' - path to local UNIX socket.</li>
%% </ul>
%%
%% Options `host' and `path' are mutually exclusive. All other options are
%% currently ignored, but the library authors reserve right to expand the set
%% of supported options in future.
%% @end
-module(logger_udp_h).

-export([adding_handler/1, changing_config/3, filter_config/1, log/2]).

% @hidden
adding_handler(Config) ->
    #{config := HConf} = Config,
    Type = type_for(HConf),
    Sockets = logger_udp_server:get_connections(Type),

    Config1 = Config#{config := HConf#{sockets => Sockets}},
    {ok, Config1}.

% @hidden
changing_config(_SetOrUpdate, _OldConf, NewConf) ->
    #{config := HConf} = NewConf,
    Type = type_for(HConf),
    Sockets = logger_udp_server:get_connections(Type),

    {ok, NewConf#{config => HConf#{sockets => Sockets}}}.

% @hidden
filter_config(Config) ->
    #{config := HConf0} = Config,
    HConf = maps:remove(sockets, HConf0),
    Config#{config := HConf}.

% @hidden
log(LogEvent, Config) ->
    #{config := HConf, formatter := {FMod, FConf}} = Config,
    Dest = destination(HConf),
    #{sockets := Socks} = HConf,
    SchedulerId = erlang:system_info(scheduler_id),
    Sock = element(SchedulerId, Socks),
    Message = FMod:format(LogEvent, FConf),
    logger_udp_server:send(Sock, Dest, Message),
    ok.

-spec type_for(map()) -> logger_udp_server:sock_type().
type_for(#{host := {_, _, _, _}, port := _Port}) -> inet;
type_for(#{host := {_, _, _, _, _, _, _, _}, port := _Port}) -> inet6;
type_for(#{path := _Path}) -> local.

%% @doc Returns destination for packets.
-spec destination(map()) ->
    inet:family_address()
    | {inet:ip_address(), inet:port_number()}.
destination(#{path := Path}) -> {local, Path};
destination(#{host := Host, port := Port}) -> {Host, Port}.
