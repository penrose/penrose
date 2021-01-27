roger
=====

The command line for penrose

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/roger.svg)](https://npmjs.org/package/roger)
[![Downloads/week](https://img.shields.io/npm/dw/roger.svg)](https://npmjs.org/package/roger)
[![License](https://img.shields.io/npm/l/roger.svg)](https://github.com/penrose/penrose/blob/master/package.json)

<!-- toc -->
* [Usage](#usage)
* [Commands](#commands)
<!-- tocstop -->
# Usage
<!-- usage -->
```sh-session
$ npm install -g roger
$ roger COMMAND
running command...
$ roger (-v|--version|version)
roger/0.0.0 darwin-x64 node-v14.8.0
$ roger --help [COMMAND]
USAGE
  $ roger COMMAND
...
```
<!-- usagestop -->
# Commands
<!-- commands -->
* [`roger hello [FILE]`](#roger-hello-file)
* [`roger help [COMMAND]`](#roger-help-command)
* [`roger watch [FILE]`](#roger-watch-file)

## `roger hello [FILE]`

describe the command here

```
USAGE
  $ roger hello [FILE]

OPTIONS
  -f, --force
  -h, --help       show CLI help
  -n, --name=name  name to print

EXAMPLE
  $ roger hello
  hello world from ./src/hello.ts!
```

_See code: [src/commands/hello.ts](https://github.com/penrose/penrose/blob/v0.0.0/src/commands/hello.ts)_

## `roger help [COMMAND]`

display help for roger

```
USAGE
  $ roger help [COMMAND]

ARGUMENTS
  COMMAND  command to show help for

OPTIONS
  --all  see all commands in CLI
```

_See code: [@oclif/plugin-help](https://github.com/oclif/plugin-help/blob/v3.2.1/src/commands/help.ts)_

## `roger watch [FILE]`

describe the command here

```
USAGE
  $ roger watch [FILE]

OPTIONS
  -f, --force
  -h, --help       show CLI help
  -n, --name=name  name to print
```

_See code: [src/commands/watch.ts](https://github.com/penrose/penrose/blob/v0.0.0/src/commands/watch.ts)_
<!-- commandsstop -->
