import * as fs from "fs";
import * as path from "path";
import {
  compileTrio,
  prepareState,
  readRegistry,
  RenderStatic,
  stepUntilConvergence,
} from "../index";

const OUTPUT = "/tmp/diagrams";
const EXAMPLES = "../../examples";
const registryPath = path.join(EXAMPLES, "registry.json");
const saveDiagrams = true;

describe("End-to-end testing of existing diagrams", () => {
  const registry = JSON.parse(fs.readFileSync(registryPath).toString());
  const trios = readRegistry(registry);
  for (const trio of trios) {
    const { name, substanceURI, domainURI, styleURI } = trio;
    const [sub, sty, dsl] = [substanceURI, styleURI, domainURI].map((uri) =>
      fs.readFileSync(path.join(EXAMPLES, uri)).toString()
    );
    test(name, async () => {
      if (saveDiagrams && !fs.existsSync(OUTPUT)) {
        fs.mkdirSync(OUTPUT);
      }
      const res = compileTrio(dsl, sub, sty);
      if (res.isOk()) {
        const state = await prepareState(res.value);
        const optimized = stepUntilConvergence(state);
        const rendered = RenderStatic(optimized);
        fs.writeFileSync(
          path.join(OUTPUT, `${name}.svg`),
          rendered.outerHTML,
          "utf8"
        );
      } else {
        fail(res.error);
      }
    });
  }
});
