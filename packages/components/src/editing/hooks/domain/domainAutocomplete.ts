import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
// import { printTree } from "@lezer-unofficial/printer";
import { SyntaxNode } from "@lezer/common";
import { useCallback } from "react";
import { DomainCache } from "../../../editing/types";

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
    // console.log(InsideDeclaration(parentNode));
    // console.log(nodeBefore);
    // console.log(leftSib);
    // console.log(parentNode);
    // console.log(context.pos);
    // console.log(context.state.doc);
    // console.log(wholeTree);
    // console.log(wholeTree.toString());
    // console.log(nodeBefore);
    // console.log(context.pos);

    // Autocomplete keywords
    if (InsideDeclaration(parentNode) && leftSib === null) {
      return {
        from: word.from,
        options: keywordOptions,
      };
    }
    /*
     * Ambiguous case. Example: "function name() -> type (newline) typ"
     * would not suggest "type" without the following code, to assist in
     * Error recovery, Lezer assumes "typ" is inside Function as a return
     * name Identifier. We manually suggest autocompletion keywords here.
     */
    if (
      parentNode != null &&
      leftSib != null &&
      (parentNode.name === "Function" || parentNode.name === "Constructor") &&
      leftSib.name === "Identifier"
    ) {
      return {
        from: word.from,
        options: keywordOptions,
      };
    }

    // Autocomplete predicate if it's following symmetric
    else if (leftSib !== null && leftSib.name === "symmetric") {
      return {
        from: word.from,
        options: [{ label: "predicate", type: "keyword" }],
      };
    }

    // Type name autocompletion
    // Check required to avoid type error in getTypeOptions
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
