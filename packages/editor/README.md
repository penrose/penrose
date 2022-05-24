# `@penrose/editor`: the Penrose IDE

`@penrose/editor` is an IDE for both end-users and Penrose developers, supporting both (1) local development mode with roger and (2) online editing mode.

# Dependencies

`@penrose/editor` depends on `@penrose/core` and `@penrose/components`.

# Local development

* Run `roger watch` in a folder that contains `.sub`, `.sty`, and `.dsl` files.
* Run `yarn start` in the monorepo root, which will show the . 
  * The root `yarn start` builds `core` and `components` first, and only listens to changes to the `editor` package. Changes to `core` and `components` will not cause live-reload in the browser.
  * If you are making changes to dependencies, run `yarn start` in this folder, and separately run `yarn watch` in `core` and/or `components` so the changes get live-reloaded.
* In local development mode, all files loaded by roger will be read-only in the IDE, but the editor will update the editor panes if there are updates to them in the local file system. You will need to use your favorite code editor to edit local files. 


