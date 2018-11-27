FROM fpco/stack-build:lts-9.10

# Build container
# docker build -t vanessa/penrose .

# Run penrose via container
# docker run vanessa/penrose
# docker run -it -p 8000:8000 -p 9160:9160 vanessa/penrose web

# Interactive debugging, or shell inside container
# docker run -it vanessa/penrose bash

LABEL Maintainer @vsoch

ENV GHCVER 8.2.1 
ENV CACHE_NAME 8.2.1
ENV BUILD_BINARY 1
ENV PATH $HOME/.local/bin:/opt/ghc/$GHCVER/bin:$HOME/bin:$PATH
ENV ARCH linux

ARG PENROSE_BRANCH=master
ARG PENROSE_REPO=https://www.github.com/penrose/penrose.git

RUN apt-get update && apt-get install -y git mini_httpd
                                        
RUN git clone -b ${PENROSE_BRANCH} ${PENROSE_REPO} && \
    cd penrose && \
    stack install alex happy


################################################################################
## Build

WORKDIR /penrose
RUN stack build && \
    stack install

EXPOSE 8000
EXPOSE 9160

ENTRYPOINT ["/penrose/docker/entrypoint.sh"]
