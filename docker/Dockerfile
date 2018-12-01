FROM fpco/stack-build:lts-11.22

# Build container
# docker build -f docker/Dockerfile -t penroseorg/penrose .

# Run penrose via container
# docker run penroseorg/penrose
# docker run -it -p 8000:8000 -p 9160:9160 penroseorg/penrose web --template linear-algebra

# Interactive debugging, or shell inside container
# docker run -it penroseorg/penrose bash

LABEL Maintainer @vsoch

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y git && \
    mkdir -p /penrose
WORKDIR /penrose

ADD . /penrose
RUN stack install alex happy && \
    stack build && \
    stack install && \
    chmod u+x /penrose/docker/entrypoint.sh 

EXPOSE 8000 9160

ENTRYPOINT ["/penrose/docker/entrypoint.sh"]
