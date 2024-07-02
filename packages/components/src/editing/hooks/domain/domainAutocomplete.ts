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

export const builtinTypes = ["String", "Number"];

const builtinTypeOptions = builtinTypes.map((type) => ({
  label: `${type}`,
  type: "variable",
  detail: "type",
}));

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

    // In erorr state, error node wraps the current node
    if (parentNode?.type.isError) {
      leftSib = parentNode.prevSibling;
      parentNode = parentNode.parent;
    }

    // Autocomplete keywords
    if (InsideDeclaration(parentNode) && leftSib === null) {
      return {
        from: word.from,
        // Include type options for subtype statements
        options: keywordOptions.concat(getTypeOptions(domainCache)),
      };
    }

    /*
     * Ambiguous case. Example: "function name() -> type (newline) typ"
     * would not suggest "type" without the following code, to assist in
     * Error recovery, Lezer assumes "typ" is inside Function as a return
     * name Identifier. We manually suggest autocompletion keywords here.
     */
    if (parentNode?.name === "Output" && leftSib?.name === "Identifier") {
      return {
        from: word.from,
        options: keywordOptions.concat(getTypeOptions(domainCache)),
      };
    }

    // Autocomplete predicate if it follows symmetric
    if (parentNode?.name === "Predicate" && leftSib?.name === "symmetric") {
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
    // Case 1: Inside parameter list
    if (leftSib === null && goUpToTarget(nodeBefore, "ParamList")) {
      return {
        from: word.from,
        options: getTypeOptions(domainCache).concat(builtinTypeOptions),
      };
    }
    // Case 2: Output type in functions and constructors
    else if (leftSib === null && goUpToTarget(nodeBefore, "Output")) {
      return {
        from: word.from,
        options: getTypeOptions(domainCache).concat(builtinTypeOptions),
      };
    }
    // Case 3: Defining subtype
    else if (parentNode?.name === "InheritanceList") {
      return {
        from: word.from,
        options: getTypeOptions(domainCache).concat(builtinTypeOptions),
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
