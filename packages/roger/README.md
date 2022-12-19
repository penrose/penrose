# oclif-hello-world

oclif example Hello World CLI

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/oclif-hello-world.svg)](https://npmjs.org/package/oclif-hello-world)
[![CircleCI](https://circleci.com/gh/oclif/hello-world/tree/main.svg?style=shield)](https://circleci.com/gh/oclif/hello-world/tree/main)
[![Downloads/week](https://img.shields.io/npm/dw/oclif-hello-world.svg)](https://npmjs.org/package/oclif-hello-world)
[![License](https://img.shields.io/npm/l/oclif-hello-world.svg)](https://github.com/oclif/hello-world/blob/main/package.json)

<!-- toc -->

- [Usage](#usage)
- [Commands](#commands)
<!-- tocstop -->

# Usage

<!-- usage -->

```sh-session
$ npm install -g @penrose/roger
$ roger COMMAND
running command...
$ roger (--version)
@penrose/roger/1.3.0 darwin-arm64 node-v16.13.0
$ roger --help [COMMAND]
USAGE
  $ roger COMMAND
...
```

<!-- usagestop -->

# Commands

<!-- commands -->

- [`roger help [COMMAND]`](#roger-help-command)
- [`roger plugins`](#roger-plugins)
- [`roger plugins:install PLUGIN...`](#roger-pluginsinstall-plugin)
- [`roger plugins:inspect PLUGIN...`](#roger-pluginsinspect-plugin)
- [`roger plugins:install PLUGIN...`](#roger-pluginsinstall-plugin-1)
- [`roger plugins:link PLUGIN`](#roger-pluginslink-plugin)
- [`roger plugins:uninstall PLUGIN...`](#roger-pluginsuninstall-plugin)
- [`roger plugins:uninstall PLUGIN...`](#roger-pluginsuninstall-plugin-1)
- [`roger plugins:uninstall PLUGIN...`](#roger-pluginsuninstall-plugin-2)
- [`roger plugins update`](#roger-plugins-update)
- [`roger watch`](#roger-watch)

## `roger help [COMMAND]`

Display help for roger.

```
USAGE
  $ roger help [COMMAND] [-n]

ARGUMENTS
  COMMAND  Command to show help for.

FLAGS
  -n, --nested-commands  Include all nested commands in the output.

DESCRIPTION
  Display help for roger.
```

_See code: [@oclif/plugin-help](https://github.com/oclif/plugin-help/blob/v5.1.12/src/commands/help.ts)_

## `roger plugins`

List installed plugins.

```
USAGE
  $ roger plugins [--core]

FLAGS
  --core  Show core plugins.

DESCRIPTION
  List installed plugins.

EXAMPLES
  $ roger plugins
```

_See code: [@oclif/plugin-plugins](https://github.com/oclif/plugin-plugins/blob/v2.1.0/src/commands/plugins/index.ts)_

## `roger plugins:install PLUGIN...`

Installs a plugin into the CLI.

```
USAGE
  $ roger plugins:install PLUGIN...

ARGUMENTS
  PLUGIN  Plugin to install.

FLAGS
  -f, --force    Run yarn install with force flag.
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Installs a plugin into the CLI.

  Can be installed from npm or a git url.

  Installation of a user-installed plugin will override a core plugin.

  e.g. If you have a core plugin that has a 'hello' command, installing a user-installed plugin with a 'hello' command
  will override the core plugin implementation. This is useful if a user needs to update core plugin functionality in
  the CLI without the need to patch and update the whole CLI.

ALIASES
  $ roger plugins add

EXAMPLES
  $ roger plugins:install myplugin

  $ roger plugins:install https://github.com/someuser/someplugin

  $ roger plugins:install someuser/someplugin
```

## `roger plugins:inspect PLUGIN...`

Displays installation properties of a plugin.

```
USAGE
  $ roger plugins:inspect PLUGIN...

ARGUMENTS
  PLUGIN  [default: .] Plugin to inspect.

FLAGS
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Displays installation properties of a plugin.

EXAMPLES
  $ roger plugins:inspect myplugin
```

## `roger plugins:install PLUGIN...`

Installs a plugin into the CLI.

```
USAGE
  $ roger plugins:install PLUGIN...

ARGUMENTS
  PLUGIN  Plugin to install.

FLAGS
  -f, --force    Run yarn install with force flag.
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Installs a plugin into the CLI.

  Can be installed from npm or a git url.

  Installation of a user-installed plugin will override a core plugin.

  e.g. If you have a core plugin that has a 'hello' command, installing a user-installed plugin with a 'hello' command
  will override the core plugin implementation. This is useful if a user needs to update core plugin functionality in
  the CLI without the need to patch and update the whole CLI.

ALIASES
  $ roger plugins add

EXAMPLES
  $ roger plugins:install myplugin

  $ roger plugins:install https://github.com/someuser/someplugin

  $ roger plugins:install someuser/someplugin
```

## `roger plugins:link PLUGIN`

Links a plugin into the CLI for development.

```
USAGE
  $ roger plugins:link PLUGIN

ARGUMENTS
  PATH  [default: .] path to plugin

FLAGS
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Links a plugin into the CLI for development.

  Installation of a linked plugin will override a user-installed or core plugin.

  e.g. If you have a user-installed or core plugin that has a 'hello' command, installing a linked plugin with a 'hello'
  command will override the user-installed or core plugin implementation. This is useful for development work.

EXAMPLES
  $ roger plugins:link myplugin
```

## `roger plugins:uninstall PLUGIN...`

Removes a plugin from the CLI.

```
USAGE
  $ roger plugins:uninstall PLUGIN...

ARGUMENTS
  PLUGIN  plugin to uninstall

FLAGS
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Removes a plugin from the CLI.

ALIASES
  $ roger plugins unlink
  $ roger plugins remove
```

## `roger plugins:uninstall PLUGIN...`

Removes a plugin from the CLI.

```
USAGE
  $ roger plugins:uninstall PLUGIN...

ARGUMENTS
  PLUGIN  plugin to uninstall

FLAGS
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Removes a plugin from the CLI.

ALIASES
  $ roger plugins unlink
  $ roger plugins remove
```

## `roger plugins:uninstall PLUGIN...`

Removes a plugin from the CLI.

```
USAGE
  $ roger plugins:uninstall PLUGIN...

ARGUMENTS
  PLUGIN  plugin to uninstall

FLAGS
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Removes a plugin from the CLI.

ALIASES
  $ roger plugins unlink
  $ roger plugins remove
```

## `roger plugins update`

Update installed plugins.

```
USAGE
  $ roger plugins update [-h] [-v]

FLAGS
  -h, --help     Show CLI help.
  -v, --verbose

DESCRIPTION
  Update installed plugins.
```

## `roger watch`

Watch the current folder for files & changes (must end in .sub,.sty,.dsl)

```
USAGE
  $ roger watch [-p <value>]

FLAGS
  -p, --port=<value>  [default: 9160] websocket port to serve to frontend

DESCRIPTION
  Watch the current folder for files & changes (must end in .sub,.sty,.dsl)

EXAMPLES
  $ roger watch
```

_See code: [dist/commands/watch.ts](https://github.com/penrose/penrose/blob/v1.3.0/dist/commands/watch.ts)_

<!-- commandsstop -->
