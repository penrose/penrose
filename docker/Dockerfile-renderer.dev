FROM node:11.10.1

WORKDIR /home/node

RUN npm config set prefix "/home/node/.npm-packages"

ENV PATH="=/home/node/.npm-packages/bin:${PATH}"

COPY ./src/react-renderer/package.json /home/node/react-renderer/package.json

WORKDIR /home/node/react-renderer

RUN npm install

ENTRYPOINT ["sh", "-c", "PORT=3500 npm start"]

EXPOSE 3500