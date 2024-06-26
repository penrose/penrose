import { LRParser } from "@lezer/lr";

export function hasNoErrors(parser: LRParser, input: string) {
  let tree = parser.parse(input);
  let hasNoErrors = true;
  tree.iterate({
    enter: (node) => {
      return true;
    },
    leave: (node) => {
      if (node.type.isError) hasNoErrors = false;
    },
  });
  return hasNoErrors;
}
