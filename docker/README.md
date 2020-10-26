# Docker Compose Workflow

## Setting Up

* Ensure docker and docker-compose are installed and the docker daemon is running.

* Ensure you have GNU Make installed (the `make` command).

* Run `make start-dev` to start the containers. If it's your first run, wait for the build to finish, and then keep the terminal running.

* Run `make dev-build` in another terminal to do initial dep, GHC installation

* To shut down the containers (while preserving your build cache), just `ctrl-C` out of the `make start-dev` terminal. If it exited by itself for some reason, you can run `make stop-dev`.

## Developing The Renderer

* Run `make start-dev` if you haven't already.

* Wait for `make start-dev` to finish such that you see the message `You can now view penrose-web in the browser`.

* Enter `make shell-dev` in another terminal and run your desired domain.

* Navigate to `localhost:3500` in the browser to see the renderer.

* Edit your files locally, and you'll get auto-reloading as usual!

## Developing The System

* Run `make start-dev` if you haven't already.

All the files in `TOP` are mounted into the `penrose` container.

Rebuild using `make dev-build`, and run penrose using `make penrose [args]` as if you were in the `src/` folder.
E,g:
```
make dev-build
make penrose linear-algebra-domain/determinants.sub linear-algebra-domain/linear-algebra.sty linear-algebra-domain/linear-algebra.dsl
```

Alternatively, use `make shell-dev` in another terminal to enter an interactive bash shell and run your stack/ghc build commands, and run penrose itself using the `penrose` command.

Note that the `penrose` command in both scenarios is aliased to include a `--domain=0.0.0.0` arg due to the way Docker works with IPs.

## Everything Broke, Restart

Just run `make recreate-dev` to recreate the environment. Note that this may require stack to reinstall all your deps when you use the system again.
