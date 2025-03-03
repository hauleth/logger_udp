<!--
SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>

SPDX-License-Identifier: Apache-2.0
-->

# `logger_udp`

[![Hex.pm](https://img.shields.io/hexpm/v/logger_udp?style=flat-square)](https://hex.pm/packages/logger_udp)
[![HexDocs](https://img.shields.io/badge/HexDocs-docs-blue?style=flat-square)](https://hexdocs.pm/logger_udp/)
[![Hex.pm License](https://img.shields.io/hexpm/l/logger_udp?style=flat-square)](https://tldrlegal.com/license/apache-license-2.0-(apache-2.0))
![REUSE Compliance](https://img.shields.io/github/actions/workflow/status/hauleth/logger_udp/erlang.yml?branch=master&style=flat-square&label=REUSE%20compliance)
[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/hauleth/logger_udp/erlang.yml?branch=master&style=flat-square)](https://github.com/hauleth/logger_udp/actions)
[![Codecov](https://img.shields.io/codecov/c/gh/hauleth/logger_udp?style=flat-square)](https://codecov.io/gh/hauleth/logger_udp)

Implementation of Erlang's `logger` handler for UDP sinks.

## Behaviour

Idea is simple - for each scheduler in the VM we start one UDP socket that is
then used for sending messages to the sink. This provides us "natural" way for
overload protection.

## Configuration

- `path` - path for local socket used for communication with logger service

or

- `host` - tuple describing IPv4 or IPv6 address
- `port` - port used for communication

All other options are currently ignored.

## Build

    $ rebar3 compile
