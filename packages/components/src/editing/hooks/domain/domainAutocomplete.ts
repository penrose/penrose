import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { SyntaxNode } from "@lezer/common";
import { useCallback } from "react";
import { DomainCache } from "../../../editing/types";

const keywordOptions = [
  "symmetric",
  "predicate",
  "type",
  "function",
  "constructor",
].map((kw) => ({ label: `${kw} `, type: "keyword" }));

const getTypeOptions = (domainCache: DomainCache) => {
  //   console.log(domainCache);
  return domainCache.typeNames.map((type) => ({
    label: `${type} `,
    type: "variable",
    detail: "type",
  }));
};

function InsideDeclaration(parentNode: SyntaxNode | null) {
  return (
    parentNode !== null &&
    (parentNode.name === "Type" ||
      parentNode.name === "Predicate" ||
      parentNode.name === "SymPred" ||
      parentNode.name === "Function" ||
      parentNode.name === "Constructor" ||
      parentNode.name === "Subtype")
  );
}

const DomainAutocomplete = (domainCache: DomainCache) => {
  return useCallback(
    async (context: CompletionContext) => {
      let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1);
      let parentNode = nodeBefore.parent;
      let leftSib = nodeBefore.prevSibling;
      let word = context.matchBefore(/\w*/);
      let wholeTree = syntaxTree(context.state).topNode;
      //   console.log(domainCache);
      // In erorr state, error node wraps the current node
      if (parentNode != null && parentNode.type.isError) {
        leftSib = parentNode.prevSibling;
        parentNode = parentNode.parent;
      }

      console.log(wholeTree.toString(), leftSib, parentNode);
      // console.log(wholeTree.toString());

      // not sure what this does, stolen from autocomplete example
      if (word == null || (word.from === word.to && !context.explicit)) {
        return null;
      }

      /*
        Autocomplete keyword only if there's nothing to the left of it
        Trying to case on if the characters are inside of some declaration
        doesn't work because by default, characters are viewed as part
        of subtype since subtype starts with Identifier 
        */
      if (InsideDeclaration(parentNode) && leftSib === null) {
        return {
          from: word.from,
          options: keywordOptions,
        };
      }
      // Exception: Autocomplete predicate if it's following symmetric
      else if (leftSib !== null && leftSib.name === "symmetric") {
        return {
          from: word.from,
          options: [{ label: "predicate", type: "keyword" }],
        };
      }

      // Type name autocompletion
      if (domainCache === null) {
        return null;
      }
      // Case 1: Inside parameter list, either as first item or after comma
      if (
        parentNode !== null &&
        parentNode.name === "ParamList" &&
        (leftSib === null || leftSib.name === "Sep")
      ) {
        return {
          from: word.from,
          options: getTypeOptions(domainCache),
        };
      }
      // Case 2: Output type in functions and constructors
      else if (leftSib !== null && leftSib.name === "ParamList") {
        return {
          from: word.from,
          options: getTypeOptions(domainCache),
        };
      }
      // Case 3: Defining subtype
      else if (parentNode !== null && parentNode.name === "InheritanceList") {
        return {
          from: word.from,
          options: getTypeOptions(domainCache),
        };
      }

      return null;
    },
    [domainCache],
    // need to specify, otherwise domainCache won't update correctly
  );
};

export default DomainAutocomplete;
