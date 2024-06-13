import { SyntaxNode } from "@lezer/common";
import { parser } from "../../parser/domain/domain";

const extractText = (domainProg: string, to: number, from: number) => {
  return domainProg.slice(from, to);
};

const getTypeNames = (domainProg: string, typeNodes: SyntaxNode[]) => {
  let types = [] as string[];

  typeNodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "type"
    nodeCursor.firstChild();
    // move to identifier or SubType
    nodeCursor.nextSibling();
    if (nodeCursor.type.name === "Identifier") {
      types.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
    } else if (nodeCursor.type.name === "Subtype") {
      // move into SubType identifier
      nodeCursor.firstChild();
      types.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
    }
  });
  //   console.log("types", types);

  return types;
};

const getPredicateNames = (
  domainProg: string,
  predNodes: SyntaxNode[],
  symPredNodes: SyntaxNode[],
) => {
  let preds = [] as string[];

  predNodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "predicate"
    nodeCursor.firstChild();
    // move to identifier
    nodeCursor.nextSibling();
    preds.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
  });

  symPredNodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "symmetric"
    nodeCursor.firstChild();
    // move into Predicate
    nodeCursor.nextSibling();
    // move into "predicate"
    nodeCursor.firstChild();
    // move to identifier
    nodeCursor.nextSibling();
    preds.push(extractText(domainProg, nodeCursor.to, nodeCursor.from));
  });
  return preds;
  //   console.log("preds", preds);
};

// Honestly this could probably be merged with getTypeNames
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
  //   console.log("fns / cons", names);
  return names;
};

export const getDomainCache = (domainProg: string) => {
  const tree = parser.parse(domainProg).topNode;

  return {
    typeNames: getTypeNames(domainProg, tree.getChildren("Type")),
    predNames: getPredicateNames(
      domainProg,
      tree.getChildren("Predicate"),
      tree.getChildren("SymPred"),
    ),
    fnNames: getFn_ConsNames(domainProg, tree.getChildren("Function")),
    consNames: getFn_ConsNames(domainProg, tree.getChildren("Constructor")),
  };
};
