% SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(sample_formatter).

-export([check_config/1, format/2]).

check_config(_Config) -> ok.

format(#{level := L, msg := {string, Str}}, _Conf) ->
    ct:pal("~s: ~s", [L, Str]),
    io_lib:format("~s ~s: ~s", [?MODULE, L, Str]).
