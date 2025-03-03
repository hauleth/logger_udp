<!--
SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>

SPDX-License-Identifier: Apache-2.0
-->

# `logger_udp`

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
