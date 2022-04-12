import { Command, flags } from "@oclif/command";
import chalk from "chalk";
import chokidar from "chokidar";
import fs from "fs";
import path from "path";
import WebSocket from "ws";
// eslint-disable-next-line node/no-unsupported-features/node-builtins
const fsp = fs.promises;

interface Args {
  [type: string]: string;
}

export default class Watch extends Command {
  static description =
    "watches files for changes; files can be passed in any order";

  static examples = [];

  static flags = {
    help: flags.help({ char: "h" }),
    port: flags.integer({
      char: "p",
      description: "websocket port to serve to frontend",
      default: 9160,
    }),
  };

  static args = [
    { name: "substance", required: true },
    { name: "style", required: true },
    { name: "domain", required: true },
  ];

  current: Args = {
    substance: "",
    style: "",
    domain: "",
  };

  currentFilenames: Args = {
    substance: "",
    style: "",
    domain: "",
  };

  wss: WebSocket.Server | undefined = undefined;

  sendFiles = async () => {
    const {
      substance: substanceFilename,
      style: styleFilename,
      domain: domainFilename,
    } = this.currentFilenames;
    const { substance, style, domain } = this.current;
    const result = {
      type: "trio",
      substance: {
        fileName: substanceFilename,
        contents: substance,
      },
      style: {
        fileName: styleFilename,
        contents: style,
      },
      domain: {
        fileName: domainFilename,
        contents: domain,
      },
    };
    this.wss?.clients.forEach((client) => {
      client.send(JSON.stringify(result));
    });
  };

  reorder = (unordered: Args): Args => {
    const ordered: Args = {};
    for (const fakeType in unordered) {
      const filename = unordered[fakeType];
      const type = { ".sub": "substance", ".sty": "style", ".dsl": "domain" }[
        path.extname(filename)
      ];
      if (!type) {
        console.error(`❌ Unrecognized file extension: ${filename}`);
        this.exit(1);
      }
      if (type in ordered) {
        console.error(
          `❌ Duplicate ${type} files: ${ordered[type]} and ${filename}`
        );
        this.exit(1);
      }
      ordered[type] = filename;
    }
    return ordered;
  };

  readFile = async (fileName: string): Promise<string | undefined> => {
    try {
      const read = await fsp.readFile(fileName, "utf8");
      return read;
    } catch (error) {
      console.error(`❌ Could not open ${fileName}: ${error}`);
      return undefined;
    }
  };

  watchFile = async (fileName: string, type: string) => {
    const watcher = chokidar.watch(fileName, {
      awaitWriteFinish: {
        pollInterval: 100,
        stabilityThreshold: 300, // increase to make file listen faster
      },
    });
    this.currentFilenames[type] = fileName;
    watcher.on("error", (err: Error) => {
      console.error(`❌ Could not open ${fileName} ${type}: ${err}`);
      this.exit(1);
    });
    watcher.on("change", async () => {
      const str = await this.readFile(fileName);
      if (str === undefined) {
        this.exit(1);
      }
      this.current[type] = str;
      console.info(
        "✅",
        chalk.blueBright(`${type}`) +
          chalk.whiteBright(` ${fileName}`) +
          chalk.blueBright(" updated, sending...")
      );
      this.sendFiles();
    });
    const str = await this.readFile(fileName);
    if (str === undefined) {
      this.exit(1);
    }
    this.current[type] = str;
  };

  async run() {
    const { args: unorderedArgs, flags } = this.parse(Watch);
    const args = this.reorder(unorderedArgs);

    console.info(chalk.blue(`💂 starting on port ${flags.port}...`));

    await this.watchFile(args.substance, "substance");
    await this.watchFile(args.style, "style");
    await this.watchFile(args.domain, "domain");

    this.wss = new WebSocket.Server({
      port: flags.port,
    });

    this.wss.on("connection", (ws) => {
      this.sendFiles();
      ws.on("message", async (m) => {
        const parsed = JSON.parse(m as string);
        if (parsed.type === "getFile") {
          const parentDir = path.parse(args.style).dir;
          const joined = path.resolve(parentDir, parsed.path);
          const contents = await this.readFile(joined);
          ws.send(JSON.stringify({ type: "gotFile", contents }));
        }
      });
    });

    await this.sendFiles();
  }
}
