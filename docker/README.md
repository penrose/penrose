# Docker Recipes

These Docker recipes are here to help with Penrose development.

 - [Dockerfile](Dockerfile) is the main build file that builds and deploys to [vanessa/penrose](https://hub.docker.com/r/vanessa/penrose/). It takes quite a long time and ideally we can figure out a better base to use.
 - [Dockerfile.local](Dockerfile.local) is the equivalent build, but from the local folder. This will produce a local development container.
 - [Dockerfile.dev](Dockerfile.dev) simply bootstraps Dockerfile.local and re-adds the entrypoint.sh file, in case there are changes to it.


