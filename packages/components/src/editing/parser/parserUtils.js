import { testTree } from "@lezer/generator/test";

export function createTestParser(parser) {
  const testParser = (input, expected) => {
    let tree = parser.parse(input);
    testTree(tree, expected);
  };

  return testParser;
}
