# Docker Compose Workflow

**TODO: IDE inclusion**

## Setting Up

* Ensure docker is installed and the daemon is running.

* Ensure you have GNU Make installed (the `make` command).

* Run `make start-dev` to start the containers.

* Run `make dev-build` in another terminal to do initial dep, GHC installation

* To shut down the containers (while preserving your build cache), just `ctrl-C` out of the `make start-dev` terminal.

## Developing The Renderer

* Wait for `make start-dev` to finish such that you see the message `You can now view react-renderer in the browser`.

* Enter `make shell-dev` in another terminal and run your desired domain.

* Navigate to `localhost:3500` in the browser to see the renderer.

* Edit your files locally, and you'll get auto-reloading as usual!

## Developing The System

All the files in `TOP` are mounted into the `penrose` container.

Just use `make shell-dev` in another terminal to enter an interactive bash shell and run your stack/ghc build commands, and run penrose itself using `runpenrose`.

## Everything Broke, Restart

Just run `make recreate-dev` to recreate the environment. Note that this may require stack to reinstall all your deps when you use the system again.