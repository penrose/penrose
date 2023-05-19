import chokidar from "chokidar";
import { promises as fs } from "fs";
import { parse, resolve } from "path";
import WS, { WebSocketServer } from "ws";

let wss: WS.Server | null = null;

const files: { substance: string[]; style: string[]; domain: string[] } = {
  substance: [],
  style: [],
  domain: [],
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
    }

    broadcastFiles();
  });
  watcher.on("error", (err) => {
    console.error(err);
  });
  watcher.on("change", async (p) => {
    if (
      ["sub", "substance", "sty", "style", "dsl", "domain"].includes(
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
      case "sty":
      case "style":
        files.style = files.style.filter((f) => f !== p);
        break;
      case "dsl":
      case "domain":
        files.domain = files.domain.filter((f) => f !== p);
        break;
    }

    broadcastFiles();
  });
}
