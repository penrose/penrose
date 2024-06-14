import { SyntaxNode, TreeCursor } from "@lezer/common";
import { parser } from "../../parser/domain/domain";

const extractText = (domainProg: string, to: number, from: number) => {
  return domainProg.slice(from, to);
};

type objDictType = { [id: string]: string };

const populateParamTypes = (
  nodeCursor: TreeCursor,
  dictObj: objDictType,
  domainProg: string,
): objDictType => {
  let i = 0;
  dictObj[i] = extractText(domainProg, nodeCursor.to, nodeCursor.from);
  while (nodeCursor.nextSibling()) {
    // Navigate to type names
    if (nodeCursor.type.name === "Sep" && nodeCursor.nextSibling()) {
      i++;
      dictObj[i] = extractText(domainProg, nodeCursor.to, nodeCursor.from);
    }
  }

  return dictObj;
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

  console.log(
    getPredsInfo(
      domainProg,
      tree.getChildren("Predicate").concat(tree.getChildren("SymPred")),
    ),
  );

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

const getPredsInfo = (domainProg: string, predNodes: SyntaxNode[]) => {
  let preds = [] as objDictType[];

  predNodes.forEach((node: SyntaxNode) => {
    let predObj = {} as { [id: string]: string };
    let nodeCursor = node.cursor();
    // move to "predicate" or symmetric
    nodeCursor.firstChild();
    if (nodeCursor.type.name === "symmetric") {
      // move into Predicate
      nodeCursor.nextSibling();
      // move into "predicate"
      nodeCursor.firstChild();
    }
    // move to identifier, set name
    nodeCursor.nextSibling();
    predObj["name"] = extractText(domainProg, nodeCursor.to, nodeCursor.from);
    // move to param list
    nodeCursor.nextSibling();
    // if in case empty
    if (nodeCursor.firstChild()) {
      predObj = populateParamTypes(nodeCursor, predObj, domainProg);
    }

    preds.push(predObj);
  });

  return preds;
};
