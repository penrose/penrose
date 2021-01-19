FROM haskell:8.0.2

RUN apt-get update \
    && apt-get -y install curl \
    && curl -sL https://deb.nodesource.com/setup_11.x | bash - \
    && apt-get install -y nodejs

RUN npm install -g typescript

RUN cd home

RUN git clone https://github.com/penrose/penrose.git

RUN cd penrose

RUN stack build

RUN cd src

RUN cd react-renderer

RUN npm install

EXPOSE 3000
EXPOSE 9160

