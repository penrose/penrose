import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { SyntaxNode } from "@lezer/common";
import { useCallback } from "react";
import { DomainCache } from "../../../editing/types";
import { goUpToTarget } from "../hooksUtils";

export const domainKws = [
  "symmetric",
  "predicate",
  "type",
  "function",
  "constructor",
];

const keywordOptions = domainKws.map((kw) => ({
  label: `${kw} `,
  type: "keyword",
}));

export const getTypeOptions = (domainCache: DomainCache) => {
  return domainCache.typeNames.map((type) => ({
    label: `${type}`,
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

// Seperated function for testing purposes (else React hook error is thrown)
export const createDomainAutocomplete = (domainCache: DomainCache) => {
  return async (context: CompletionContext) => {
    let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1);
    let parentNode = nodeBefore.parent;
    let leftSib = nodeBefore.prevSibling;
    // /\w*/ is regex any number of alphanumeric and underscore characters
    let word = context.matchBefore(/\w*/);

    if (word == null || (word.from == word.to && !context.explicit)) {
      return null;
    }

    let wholeTree = syntaxTree(context.state).topNode;

    // In erorr state, error node wraps the current node
    if (parentNode != null && parentNode.type.isError) {
      leftSib = parentNode.prevSibling;
      parentNode = parentNode.parent;
    }

    // console.log(context.pos, context.state.doc.length);
    // console.log(printTree(wholeTree, context.state.doc.toString()));

    /*
     * Autocomplete predicate if it's following symmetric
     * Lezer guesses partial is a separate declaration node
     * Example: symmetric p
     * Lezer guesses this is an errored predicate node and the p belongs to a
     * new subtype node
     * Order matters, keyword autocomplete also checks true in this case
     */
    if (
      // Check current node follows a Predicate declaration
      parentNode != null &&
      parentNode.prevSibling != null &&
      parentNode.prevSibling.name === "Predicate"
    ) {
      if (
        // Check has symmetric and is incomplete
        parentNode.prevSibling.firstChild != null &&
        parentNode.prevSibling.firstChild.name == "symmetric" &&
        parentNode.prevSibling.firstChild.nextSibling != null &&
        parentNode.prevSibling.firstChild.nextSibling.type.isError
      ) {
        return {
          from: word.from,
          options: [{ label: "predicate", type: "keyword" }],
        };
      }
    }

    // Autocomplete keywords
    if (InsideDeclaration(parentNode) && leftSib === null) {
      return {
        from: word.from,
        options: keywordOptions.concat(getTypeOptions(domainCache)),
      };
    }

    // Type name autocompletion
    // Check required to avoid type error in getTypeOptions
    if (domainCache === null) {
      return null;
    }
    // Case 1: Inside parameter list
    if (leftSib === null && goUpToTarget(nodeBefore, "ParamList")) {
      return {
        from: word.from,
        options: getTypeOptions(domainCache),
      };
    }
    // Case 2: Output type in functions and constructors
    else if (leftSib === null && goUpToTarget(nodeBefore, "Output")) {
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
  };
};

const DomainAutocomplete = (domainCache: DomainCache) => {
  return useCallback(
    createDomainAutocomplete(domainCache),
    [domainCache],
    // Need to specify, otherwise domainCache won't update correctly
  );
};

export default DomainAutocomplete;
