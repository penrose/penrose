import {
  compileTrio,
  prepareState,
  RenderStatic,
  showError,
  stepUntilConvergence,
} from "@penrose/core";
import * as fs from "fs";
import * as path from "path";
import prettier from "prettier";

interface StyledTrio {
  substance: string;
  style: string[];
  domain: string;
}

export default async function (name: string, variation?: string) {
  const paths = JSON.parse(
    await fs.readFileSync(name).toString()
  ) as StyledTrio;
  const domain = fs.readFileSync(paths.domain).toString();
  const substance = fs.readFileSync(paths.substance).toString();
  const styles = paths.style.map((style) => fs.readFileSync(style).toString());
  const combinedStyle = styles.join("\n");

  const compilerOutput = await compileTrio({
    substance,
    style: combinedStyle,
    domain,
    variation: variation ?? "default",
  });
  if (compilerOutput.isErr()) {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }
  const compiledState = compilerOutput.value;
  const initialState = await prepareState(compiledState);
  let optimizedState;
  const optimizedOutput = stepUntilConvergence(initialState, 10000);
  if (optimizedOutput.isOk()) {
    optimizedState = optimizedOutput.value;
  } else {
    throw new Error(
      `Optimization failed:\n${showError(optimizedOutput.error)}`
    );
  }
  const resolvePath = async (filePath: string) => {
    // Handle absolute URLs
    if (/^(http|https):\/\/[^ "]+$/.test(filePath)) {
      const fileURL = new URL(filePath).href;
      try {
        const fileReq = await fetch(fileURL);
        return fileReq.text();
      } catch (e) {
        return undefined;
      }
    }

    // Relative paths
    // HACK: treat the first style's directory as the root directory for image imports
    const parentDir = path.parse(paths.style[0]).dir;
    const joined = path.resolve(parentDir, filePath);
    return fs.readFileSync(joined, "utf8").toString();
  };
  const canvas = (await RenderStatic(optimizedState, resolvePath, "automator"))
    .outerHTML;

  // write to file
  const out = "./diagram.svg";
  const parentFolder = path.dirname(out);
  if (!fs.existsSync(parentFolder)) {
    fs.mkdirSync(parentFolder, { recursive: true });
  }
  fs.writeFileSync(out, prettier.format(canvas, { parser: "html" }));
  console.log(`The diagram has been saved as ${out}`);
}
