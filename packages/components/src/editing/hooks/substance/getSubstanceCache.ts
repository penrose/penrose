import { SyntaxNode } from "@lezer/common";
import { parser } from "../../parser/substance/substance";
import { extractText } from "../hooksUtils";

const containsIndexedStatement = (node: SyntaxNode): boolean => {
  return node.getChild("IndexedStatement") !== null;
};

const getIdsFromTypes = (substanceProg: string, typeAppNodes: SyntaxNode[]) => {
  let ids = [] as string[];

  typeAppNodes.forEach((node: SyntaxNode) => {
    // Ignore indexed statements
    if (containsIndexedStatement(node)) {
      // exits from current iteration, like continue
      return;
    }

    let nodeCursor = node.cursor();
    // move to NamedId
    nodeCursor.firstChild();
    // move to identifier, loop because of comma sep list
    while (nodeCursor.nextSibling()) {
      if (nodeCursor.name === "Identifier") {
        ids.push(extractText(substanceProg, nodeCursor.to, nodeCursor.from));
      }
    }
  });
  //   console.log(ids);
  return ids;
};

const getIdsFromFnCons = (
  substanceProg: string,
  fnConsAppNodes: SyntaxNode[],
) => {
  let ids = [] as string[];

  fnConsAppNodes.forEach((node: SyntaxNode) => {
    // Ignore indexed statements
    if (containsIndexedStatement(node)) {
      // exits from current iteration, like continue
      return;
    }

    let nodeCursor = node.cursor();
    // move to Identifier, NamedId, or Let
    nodeCursor.firstChild();
    // Case 1: object_name := function_constructor_name (argument_list)
    // here object_name must already be declared so we don't double count it
    if (nodeCursor.type.name === "Identifier") {
      return;
    }
    // Case 2: Let object_name := function_constructor_name (argument_list)
    // or type_name object_name := function_constructor_name (argument_list)
    nodeCursor.nextSibling();
    // Move to Identifier
    ids.push(extractText(substanceProg, nodeCursor.to, nodeCursor.from));
  });
  //   console.log(ids);
  return ids;
};

export const getSubstanceCache = (substanceProg: string) => {
  const tree = parser.parse(substanceProg).topNode;
  const idsFromTypes = getIdsFromTypes(
    substanceProg,
    tree.getChildren("TypeApp"),
  );
  const idsFromFnCons = getIdsFromFnCons(
    substanceProg,
    tree.getChildren("Fn_ConsApp"),
  );

  return { varNames: idsFromTypes.concat(idsFromFnCons) };
};
