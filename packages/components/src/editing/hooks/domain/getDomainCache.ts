import { SyntaxNode } from "@lezer/common";
import { parser } from "../../parser/domain/domain";
import { extractText } from "../hooksUtils";

const getTypeNames = (domainProg: string, typeNodes: SyntaxNode[]) => {
  let types = [] as string[];

  typeNodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "type"
    nodeCursor.firstChild();
    // move to identifier
    nodeCursor.nextSibling();

    if (nodeCursor.type.name === "Identifier") {
      types.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
    }
  });

  return types;
};

const getPredicateNames = (domainProg: string, predNodes: SyntaxNode[]) => {
  let preds = [] as string[];

  predNodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "predicate" or symmetric
    nodeCursor.firstChild();
    if (nodeCursor.name === "symmetric") {
      nodeCursor.nextSibling();
    }
    // move to identifier
    nodeCursor.nextSibling();
    preds.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
  });

  return preds;
};

const getFn_ConsNames = (domainProg: string, nodes: SyntaxNode[]) => {
  let names = [] as string[];

  nodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "function" or "constructor"
    nodeCursor.firstChild();
    // move to identifier
    nodeCursor.nextSibling();
    if (nodeCursor.type.name === "Identifier") {
      names.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
    }
  });
  return names;
};

export const getDomainCache = (domainProg: string) => {
  const tree = parser.parse(domainProg).topNode;

  return {
    typeNames: getTypeNames(domainProg, tree.getChildren("TypeDecl")),
    predNames: getPredicateNames(domainProg, tree.getChildren("Predicate")),
    fnNames: getFn_ConsNames(domainProg, tree.getChildren("Function")),
    consNames: getFn_ConsNames(domainProg, tree.getChildren("Constructor")),
  };
};
