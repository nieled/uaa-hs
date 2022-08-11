# uaa-hs

[![build](https://github.com/nieled/uaa-hs/actions/workflows/build.yml/badge.svg)](https://github.com/nieled/uaa-hs/actions/workflows/build.yml)

# Nix for dev environment

## PostgreSQL

The nix-shell config has a shellHook that initializes and starts the database.

## Redis

In-memory kv store. We use it to store session ids to user ids mappings. Dev server can be started in a nix shell with:

```
redis-server
```

## RabbitMQ

Queueing system used primarily to offload tasks that are
not necessary to be processed immediately. That allows application to be more responsive to the user. Dev server can be started in a nix shell with:

```
rabbitmq-server start
```
