import * as fs from "fs";
import fetch, { Headers, Request, Response } from "node-fetch";
import * as url from "url";
import { TextDecoder, TextEncoder } from "util";

// https://stackoverflow.com/a/57943686
global.TextDecoder = TextDecoder;
global.TextEncoder = TextEncoder;

// https://www.npmjs.com/package/node-fetch/v/3.3.0
global.Headers = Headers;
global.Request = Request;
global.Response = Response;

// https://gist.github.com/joshua-gould/58e1b114a67127273eef239ec0af8989
global.fetch = (resource, options) => {
  const request = new Request(resource, options);
  if (request.url.startsWith("file:")) {
    return new Promise((resolve, reject) => {
      const filePath = url.fileURLToPath(resource);
      if (!fs.existsSync(filePath)) {
        reject(`File not found: ${filePath}`);
      }
      const readStream = fs.createReadStream(filePath);
      readStream.on("open", () => {
        resolve(
          new Response(readStream, {
            url: request.url,
            status: 200,
            statusText: "OK",
            size: fs.statSync(filePath).size,
            timeout: request.timeout,
          })
        );
      });
    });
  } else {
    return fetch(resource, options);
  }
};
