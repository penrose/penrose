# @penrose/roger

`roger` is the command-line interface for the Penrose platform. Currently, it is mostly used for local development. `roger` serves and watches changes to Domain, Substance, and Style files in the local file system.

<!-- toc -->

- [@penrose/roger](#penroseroger)
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
@penrose/roger/2.1.1 darwin-arm64 node-v18.12.1
$ roger --help [COMMAND]
USAGE
  $ roger COMMAND
...
```

<!-- usagestop -->

# Commands

<!-- commands -->

- [`roger watch`](#roger-watch)

## `roger watch`

Watch the current folder for files & changes (must end in .sub,.substance,.sty,.style,.dsl,.domain)

```
USAGE
  $ roger watch [-p <value>]

FLAGS
  -p, --port=<value>  [default: 9160] websocket port to serve to frontend

DESCRIPTION
  Watch the current folder for files & changes (must end in .sub,.substance,.sty,.style,.dsl,.domain)

EXAMPLES
  $ roger watch
```

_See code: [dist/commands/watch.js](https://github.com/penrose/penrose/blob/v2.1.1/dist/commands/watch.js)_

<!-- commandsstop -->
