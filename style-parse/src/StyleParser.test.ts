// const grammar = require("./Style.ne");
import * as nearley from "nearley";
import grammar from "./StyleParser";

const util = require("util");
const exec = util.promisify(require("child_process").exec);

async function compileNearley() {
  const { stdout, stderr } = await exec(
    "nearleyc src/Style.ne > src/StyleParser.ts"
  );
  if (stdout.length > 0) console.log("stdout:", stdout);
  if (stderr.length > 0) console.log("stderr:", stderr);
}
beforeEach(async () => {
  await compileNearley();
});

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

test("empty program", () => {
  parser.feed("");
});

test("empty block", () => {
  parser.feed("const { }");
});

test("comment and empty block", () => {
  const prog = `
  -- this is a comment
  forall Set A, B { }
  
  forall Set A, \`B\`; Map f`;
  parser.feed(prog);
});

// parser.feed(prog);
// console.log(JSON.stringify(parser.results[0]));
