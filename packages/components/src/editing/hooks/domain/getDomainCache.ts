import { SyntaxNode } from "@lezer/common";
import { domainParser as parser } from "@penrose/core";
import { extractText } from "../hooksUtils";

const getNames = (domainProg: string, nodes: SyntaxNode[]) => {
  let names = [] as string[];

  nodes.forEach((node: SyntaxNode) => {
    const id = node.getChild("Identifier");
    if (id != null) {
      names.push(extractText(domainProg, id.to, id.from));
    }
  });

  return names;
};

export const getDomainCache = (domainProg: string) => {
  const tree = parser.parse(domainProg).topNode;

  return {
    typeNames: getNames(domainProg, tree.getChildren("TypeDecl")),
    predNames: getNames(domainProg, tree.getChildren("Predicate")),
    fnNames: getNames(domainProg, tree.getChildren("Function")),
    consNames: getNames(domainProg, tree.getChildren("Constructor")),
  };
};
