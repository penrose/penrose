// require("global-jsdom/register");
import { Command, flags } from "@oclif/command";
import chokidar from "chokidar";
import fs from "fs";
import chalk from "chalk";
// eslint-disable-next-line node/no-unsupported-features/node-builtins
const fsp = fs.promises;

/*
    TODO: write error-handling function that stops the compilation process 
          and outputs errors to console
*/

import {
  //  RenderShape,
  compileTrio,
  //  RenderStatic
} from "@penrose/core";

export default class ToSvg extends Command {
  static description = "compiles to svg locally";

  static examples = [];

  static flags = {
    help: flags.help({ char: "h" }),
    once: flags.boolean({
      char: "o",
      description: "compile once without continuously watching for changes",
      default: true,
    }),
  };

  static args = [
    { name: "substance", required: true },
    { name: "style", required: true },
    { name: "domain", required: true },
    { name: "svgPath", required: false },
  ];

  current: { [type: string]: string } = {
    substance: "",
    style: "",
    domain: "",
    svgPath: "output.svg",
  };

  currentFilenames: { [type: string]: string } = {
    substance: "",
    style: "",
    domain: "",
    svgPath: "output.svg",
  };

  compileSVG = ({ shapes, labelCache }: any): any => {
    // const svg = this.fakeDocument.createElementNS(
    //   "http://www.w3.org/2000/svg",
    //   "svg"
    // );
    // svg.setAttribute("width", "100%");
    // svg.setAttribute("height", "100%");
    // svg.setAttribute("version", "1.2");
    // svg.setAttribute("viewBox", `0 0 ${1000} ${1000}`); // default to 1000 1000, will add options
    // svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    // shapes.forEach((shape: any) =>
    //   svg.appendChild(RenderShape(shape, labelCache))
    // );
    // return RenderStatic({shapes, labelCache});
  };

  sendFiles = async () => {
    const {
      substance: substanceFilename,
      style: styleFilename,
      domain: domainFilename,
    } = this.currentFilenames;
    const { substance, style, domain } = this.current;

    // this is where the code is hitting an issue:
    // @penrose/core doesn't properly compile because the module mathjax-full
    // references the browser's window object
    // and a cli too has no such object
    // Therefore, any reference to penrose functions
    // like compileTrio
    // will stop the command from compiling

    // you can see the error msg with
    // DEBUG=* npx roger

    const ahhhhh = compileTrio(domain, substance, style);
    // console.log(ahhhhh);

    // if (result.variant == "Ok") {
    //   let svg = this.compileSVG(result);
    //   console.info(svg);
    // }

    // this is probably where you'd call RenderShape

    // this.wss?.clients.forEach((client) => {
    //   client.send(JSON.stringify(result));
    // });

    // *write to file svgPath*

    return;
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
    const { args, flags } = this.parse(ToSvg);

    console.info(chalk.blue(`compiling ${args}`));

    await this.watchFile(args.substance, "substance");
    await this.watchFile(args.style, "style");
    await this.watchFile(args.domain, "domain");

    await this.sendFiles();
  }
}
