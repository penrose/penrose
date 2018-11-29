# Docker Recipes

These Docker recipes are here to help with Penrose development.

 - [Dockerfile](Dockerfile) is the main build file that builds and deploys to [penroseorg/penrose](https://hub.docker.com/r/penroseorg/penrose/). It takes quite a long time and ideally we can figure out a better base to use.
 - [Dockerfile.dev](Dockerfile.dev) simply bootstraps `penroseorg/penrose` (the container built from the recipe above) and re-adds the entrypoint.sh file along with other static files, in case there are changes to them.


