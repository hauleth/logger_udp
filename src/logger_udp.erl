-module(logger_udp).

-export([adding_handler/1, log/2]).

adding_handler(Config) ->
    #{config := HConf0} = Config,
    Type = type_for(HConf0),
    Sockets = logger_udp_server:get_connections(Type),

    Config1 = Config#{config := HConf0#{sockets => Sockets}},
    {ok, Config1}.

log(LogEvent, Config) ->
    #{config := HConf, formatter := {FMod, FConf}} = Config,
    #{sockets := Socks} = HConf,
    SchedulerId = erlang:system_info(scheduler_id),
    #{SchedulerId := Sock} = Socks,
    Message = FMod:format(LogEvent, FConf),
    logger_udp_server:send(Sock, {local, "sock"}, Message),
    ok.

type_for(#{type := Type}) when Type =:= inet, Type =:= inet6 -> Type;
type_for(#{host := {_, _, _, _}, port := _Port}) -> inet;
type_for(#{host := {_, _, _, _, _, _, _, _}, port := _Port}) -> inet6;
type_for(#{socket := _Path}) -> local.
