penrose-cli
===========

CLI for using Penrose

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/penrose-cli.svg)](https://npmjs.org/package/penrose-cli)
[![Downloads/week](https://img.shields.io/npm/dw/penrose-cli.svg)](https://npmjs.org/package/penrose-cli)
[![License](https://img.shields.io/npm/l/penrose-cli.svg)](https://github.com/penrose/penrose/blob/master/package.json)

<!-- toc -->
* [Usage](#usage)
* [Commands](#commands)
<!-- tocstop -->
# Usage
<!-- usage -->
```sh-session
$ npm install -g penrose-cli
$ penrose-cli COMMAND
running command...
$ penrose-cli (-v|--version|version)
penrose-cli/0.0.0 darwin-x64 node-v14.8.0
$ penrose-cli --help [COMMAND]
USAGE
  $ penrose-cli COMMAND
...
```
<!-- usagestop -->
# Commands
<!-- commands -->
* [`penrose-cli help [COMMAND]`](#penrose-cli-help-command)
* [`penrose-cli watch SUBSTANCE STYLE DOMAIN`](#penrose-cli-watch-substance-style-domain)

## `penrose-cli help [COMMAND]`

display help for penrose-cli

```
USAGE
  $ penrose-cli help [COMMAND]

ARGUMENTS
  COMMAND  command to show help for

OPTIONS
  --all  see all commands in CLI
```

_See code: [@oclif/plugin-help](https://github.com/oclif/plugin-help/blob/v3.2.0/src/commands/help.ts)_

## `penrose-cli watch SUBSTANCE STYLE DOMAIN`

watches files for changes

```
USAGE
  $ penrose-cli watch SUBSTANCE STYLE DOMAIN

OPTIONS
  -h, --help       show CLI help
  -p, --port=port  [default: 9160] websocket port to serve to frontend
```

_See code: [src/commands/watch.ts](https://github.com/penrose/penrose/blob/v0.0.0/src/commands/watch.ts)_
<!-- commandsstop -->
