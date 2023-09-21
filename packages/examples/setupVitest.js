import * as fs from "fs/promises";

fetch = async (url) => {
  if (url.href.startsWith("file:")) {
    // Extract the local file path from the URL
    const localFilePath = url.href.replace("file://", "");

    try {
      // Read the local file
      const data = await fs.readFile(localFilePath);

      // Return a response-like object
      return new Response(data, {
        status: 200,
        headers: { "Content-Type": "application/wasm" },
      });
    } catch (error) {
      return {
        ok: false,
        status: 404,
        text: () => Promise.resolve("Not Found"),
        // ... handle other errors as needed
      };
    }
  } else throw Error("Only file:// URLs are supported");
};
