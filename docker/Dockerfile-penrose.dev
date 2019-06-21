FROM fpco/stack-build:lts-13.24

RUN echo 'alias runpenrose=""' >> ~/.bashrc

# Put runpenrose in PATH with default domain arg (to be docker compatible)
RUN echo '/root/.local/bin/penrose --domain=0.0.0.0 "$@"' > /usr/bin/penrose && \
    chmod +x /usr/bin/penrose

WORKDIR /home/penrose/src

ENV PATH="/usr/local/cuda-10.0/bin:/opt/ghc/8.6.5/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cabal/bin"
EXPOSE 9160
