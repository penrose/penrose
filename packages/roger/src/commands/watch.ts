import { Command, flags } from "@oclif/command";
import WebSocket from "ws";
import chokidar from "chokidar";
import fs from "fs";
import chalk from "chalk";
// eslint-disable-next-line node/no-unsupported-features/node-builtins
const fsp = fs.promises;

export default class Watch extends Command {
  static description = "watches files for changes";

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

  current: { [type: string]: string } = {
    substance: "",
    style: "",
    domain: "",
  };

  currentFilenames: { [type: string]: string } = {
    substance: "",
    style: "",
    domain: "",
  };

  wss: WebSocket.Server | null = null;

  sendFiles = async () => {
    const {
      substance: substanceFilename,
      style: styleFilename,
      domain: domainFilename,
    } = this.currentFilenames;
    const { substance, style, domain } = this.current;
    const result = {
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

  readFile = async (fileName: string) => {
    try {
      const read = await fsp.readFile(fileName, "utf8");
      return read;
    } catch (error) {
      console.error(`❌ Could not open ${fileName}: ${error}`);
      this.exit(1);
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
    this.current[type] = str;
  };

  async run() {
    const { args, flags } = this.parse(Watch);

    console.info(chalk.blue(`💂 starting on port ${flags.port}...`));

    await this.watchFile(args.substance, "substance");
    await this.watchFile(args.style, "style");
    await this.watchFile(args.domain, "domain");

    this.wss = new WebSocket.Server({
      port: flags.port,
    });

    this.wss.on("connection", (ws) => {
      this.sendFiles();
      ws.on("message", (m) => {
        const parsed = JSON.parse(m as string);
        console.log("got message", parsed);
      });
    });

    await this.sendFiles();
  }
}
