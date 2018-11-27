FROM fpco/stack-build

# Build container
# docker build -t vanessa/penrose .

# Run penrose via container
# docker run vanessa/penrose

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

RUN apt-get update && apt-get install -y git ghc-8.2.1
                                        
################################################################################
## Install GHC and Stack

RUN wget -O ghr.zip https://github.com/tcnksm/ghr/releases/download/v0.5.4/ghr_v0.5.4_${ARCH}_386.zip && \
    mkdir -p $HOME/bin && \
    unzip ghr.zip -d $HOME/bin && \
    rm ghr.zip && \
    mkdir -p ~/.local/bin && \
    curl -sL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack' && \
    stack --no-terminal setup

RUN git clone -b ${PENROSE_BRANCH} ${PENROSE_REPO} && \
    cd penrose && \
    stack install alex happy


################################################################################
## Build

WORKDIR /penrose
RUN stack build && \
    stack install

ENTRYPOINT ["/root/.local/bin/penrose"]
