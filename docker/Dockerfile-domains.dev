FROM node:11.10.1

WORKDIR /home/node

RUN npm config set prefix "/home/node/.npm-packages"

ENV PATH="=/home/node/.npm-packages/bin:${PATH}"

WORKDIR /home/node/src

ENTRYPOINT ["sh", "-c", "npx http-server . -p 9090 -d false -i false --cors -c-1"]

EXPOSE 9090