FROM fpco/stack-build:lts-9.10

# Build container
# docker build -t vanessa/penrose .

# Run penrose via container
# docker run vanessa/penrose
# docker run -it -p 8000:8000 -p 9160:9160 vanessa/penrose web linear-algebra

# Interactive debugging, or shell inside container
# docker run -it vanessa/penrose bash

LABEL Maintainer @vsoch

ENV DEBIAN_FRONTEND noninteractive
ENV GHCVER 8.2.1 
ENV CACHE_NAME 8.2.1
ENV BUILD_BINARY 1
ENV PATH $HOME/.local/bin:/opt/ghc/$GHCVER/bin:$HOME/bin:$PATH
ENV ARCH linux

RUN apt-get update && apt-get install -y git && \
    mkdir -p /penrose
WORKDIR /penrose

ADD . /penrose
RUN stack install --system-ghc alex happy && \
    stack build --system-ghc && \
    stack install --system-ghc && \
    chmod u+x /penrose/docker/entrypoint.sh 

EXPOSE 8000 9160

ENTRYPOINT ["/penrose/docker/entrypoint.sh"]
