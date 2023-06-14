import chokidar from "chokidar";
import { promises as fs } from "fs";
import { parse, relative, resolve } from "path";
import WS, { WebSocketServer } from "ws";

let wss: WS.Server | null = null;

const files: {
  substance: string[];
  style: string[];
  domain: string[];
  trio: string[];
} = {
  substance: [],
  style: [],
  domain: [],
  trio: [],
};

const broadcastFiles = () => {
  if (wss) {
    for (const ws of wss.clients) {
      ws.send(JSON.stringify({ kind: "files", files: files }));
    }
  } else {
    console.warn("Websocket server not defined");
  }
};

const broadcastFileChange = async (fileName: string, token?: string) => {
  try {
    const contents = await fs.readFile(fileName, "utf8");
    if (wss) {
      for (const ws of wss.clients) {
        ws.send(
          JSON.stringify({ kind: "file_change", fileName, contents, token })
        );
      }
    } else {
      console.warn("Websocket server not defined");
    }
  } catch (error) {
    console.warn(error);
  }
};

export default async function (port = 9160): Promise<void> {
  wss = new WebSocketServer({ port });
  console.log(`watching on port ${port}`);
  wss.on("connection", (ws) => {
    console.info("client connected");
    broadcastFiles();
    ws.on("message", async (data) => {
      const parsed = JSON.parse(data.toString());
      switch (parsed.kind) {
        case "retrieve_file":
          broadcastFileChange(parsed.fileName);
          break;
        case "retrieve_file_from_style":
          const { stylePath, relativePath, token } = parsed;
          const parentDir = parse(stylePath).dir;
          const joined = resolve(parentDir, relativePath);
          broadcastFileChange(joined, token);
          break;
        case "retrieve_trio": {
          const { path, token } = parsed;
          // get containing dir of the trio file
          const parentDir = parse(path).dir;
          const contents = await fs.readFile(path, "utf8");
          const { substance, style, domain } = JSON.parse(contents);
          let combinedStyle = "";
          // concat all the style files
          for (const s of style) {
            const styPath = resolve(parentDir, s);
            combinedStyle += `-- ${styPath}\n`;
            const sty = await fs.readFile(styPath, "utf8");
            combinedStyle += sty + "\n";
          }
          // send all files
          if (wss) {
            const domainPath = resolve(parentDir, domain);
            const domainText = await fs.readFile(domainPath, "utf8");
            const substancePath = resolve(parentDir, substance);
            const substanceText = await fs.readFile(substancePath, "utf8");
            for (const ws of wss.clients) {
              ws.send(
                JSON.stringify({
                  kind: "trio_file",
                  type: "domain",
                  fileName: relative(".", domainPath),
                  contents: domainText,
                  token,
                })
              );
              ws.send(
                JSON.stringify({
                  kind: "trio_file",
                  type: "substance",
                  fileName: relative(".", substancePath),
                  contents: substanceText,
                  token,
                })
              );
              ws.send(
                JSON.stringify({
                  kind: "trio_file",
                  type: "style",
                  contents: combinedStyle,
                  fileName:
                    style.length > 1
                      ? "combined_style"
                      : relative(".", resolve(parentDir, style[0])), // HACK: use the first style path as the path to the combined Style because the combined Style doesn't exist in the file system
                  token,
                })
              );
            }
          } else {
            console.warn("Websocket server not defined");
          }
          break;
        }
        default:
          console.error(`unknown message kind: ${parsed.kind}`);
      }
    });
    ws.on("close", () => {
      console.info("client disconnected");
    });
  });

  const watcher = chokidar.watch(".", { persistent: true });
  watcher.on("add", (p) => {
    switch (p.split(".").pop()) {
      case "sub":
      case "substance":
        files.substance.push(p);
        break;
      case "sty":
      case "style":
        files.style.push(p);
        break;
      case "dsl":
      case "domain":
        files.domain.push(p);
        break;
      case "json":
        // filter out non-trio json files
        if (p.split(".").slice(1).join(".") === "trio.json") files.trio.push(p);
    }

    broadcastFiles();
  });
  watcher.on("error", (err) => {
    console.error(err);
  });
  watcher.on("change", async (p) => {
    if (
      ["json", "sub", "substance", "sty", "style", "dsl", "domain"].includes(
        p.split(".").pop() ?? ""
      )
    ) {
      console.info(`file ${p} changed`);
      broadcastFileChange(p);
    }
  });
  watcher.on("unlink", (p) => {
    switch (p.split(".").pop()) {
      case "sub":
      case "substance":
        files.substance = files.substance.filter((f) => f !== p);
        break;
      case ".sty":
      case "style":
        files.style = files.style.filter((f) => f !== p);
        break;
      case "dsl":
      case "domain":
        files.domain = files.domain.filter((f) => f !== p);
        break;
      case "json":
        files.trio = files.trio.filter((f) => f !== p);
        break;
    }

    broadcastFiles();
  });
}
