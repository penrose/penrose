import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { SyntaxNode } from "@lezer/common";
import { useCallback } from "react";
import { DomainCache, SubstanceCache } from "../../../editing/types";

const keywordOptions = ["Let", "AutoLabel", "Label", "NoLabel"].map((kw) => ({
  label: `${kw}`,
  type: "keyword",
}));

function InsideStatement(parentNode: SyntaxNode | null) {
  return (
    parentNode !== null &&
    (parentNode.name === "TypeApp" ||
      parentNode.name === "PredicateApp" ||
      parentNode.name === "Fn_ConsApp" ||
      parentNode.name === "Labeling")
  );
}

// Nested version to deal with NamedId wrapper
function InsideStatementNested(parentNode: SyntaxNode | null) {
  return (
    parentNode !== null &&
    (InsideStatement(parentNode) ||
      (parentNode.name === "NamedId" && InsideStatement(parentNode.parent)))
  );
}

const predTypeOptions = (domainCache: DomainCache) => {
  const typeOptions = domainCache.typeNames.map((type) => ({
    label: `${type} `,
    type: "type",
    detail: "type",
  }));

  const predicateOptions = domainCache.predNames.map((type) => ({
    label: `${type} `,
    type: "type",
    detail: "predicate",
  }));

  return typeOptions.concat(predicateOptions);
};

const fnConsOptions = (domainCache: DomainCache) => {
  const fnOptions = domainCache.fnNames.map((type) => ({
    label: `${type} `,
    type: "type",
    detail: "function",
  }));

  const consOptions = domainCache.consNames.map((type) => ({
    label: `${type} `,
    type: "type",
    detail: "constructor",
  }));

  return fnOptions.concat(consOptions);
};

const idOptions = (substanceCache: SubstanceCache) => {
  console.log(substanceCache);
  return substanceCache.varNames
    .map((id) => ({
      label: `${id}`,
      type: "variable",
    }))
    .filter((obj) => obj.label.length > 1);
};

const SubstanceAutocomplete = (
  domainCache: DomainCache,
  substanceCache: SubstanceCache,
) => {
  return useCallback(
    async (context: CompletionContext) => {
      let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1);
      let parentNode = nodeBefore.parent;
      let leftSib = nodeBefore.prevSibling;
      // In erorr state, error node wraps the current node
      if (parentNode != null && parentNode.type.isError) {
        leftSib = parentNode.prevSibling;
        parentNode = parentNode.parent;
      }

      let word = context.matchBefore(/\w*/);
      let wholeTree = syntaxTree(context.state).topNode;
      //   console.log(domainCache);
      //   console.log(parentNode, leftSib, wholeTree.toString());
      //   console.log(wholeTree.toString());
      // not sure what this does, stolen from autocomplete example
      if (word == null || (word.from === word.to && !context.explicit)) {
        return null;
      }

      /*
        Autocomplete keyword only if there's nothing to the left of it
        */
      if (leftSib === null && InsideStatementNested(parentNode)) {
        // keyword, predicate, or type
        const autocompleteOptions = keywordOptions.concat(
          predTypeOptions(domainCache),
        );
        return {
          from: word.from,
          options: autocompleteOptions,
        };
      } else if (leftSib !== null && leftSib.name === "AutoLabel") {
        return {
          from: word.from,
          options: [{ label: "All", type: "keyword" }].concat(
            idOptions(substanceCache),
          ),
        };
      } else if (leftSib !== null && leftSib.name === "Range") {
        return {
          from: word.from,
          options: [{ label: "where ", type: "keyword" }],
        };
      }
      // for suggestor, will suggest in correct spaces in all but Label statements
      else if (
        InsideStatementNested(parentNode) &&
        leftSib !== null &&
        // To suggest after TypeApp
        (leftSib.name === "Identifier" ||
          // To suggest after PredicateApp and Fn_ConsApp
          leftSib.name === "ArgList")
      ) {
        return {
          from: word.from,
          options: [{ label: "for ", type: "keyword" }],
        };
      }

      // Suggest function and constructor names
      if (leftSib !== null && leftSib.name === "Assignment") {
        return {
          from: word.from,
          options: fnConsOptions(domainCache),
        };
      }

      // Suggest ids
      if (
        parentNode !== null &&
        // Case 1: Inside arg list
        (parentNode.name === "ArgList" ||
          // Case 2: Inside label statement, after the keyword ("AutoLabel" etc)
          // This will "incorrectly" trigger for the label in Label statements
          // but those are supposed to be Strings or TeX anyways
          (parentNode.name === "Labeling" && leftSib !== null))
      ) {
        // console.log(substanceCache);
        return {
          from: word.from,
          options: idOptions(substanceCache),
        };
      }
      return null;
    },
    [domainCache, substanceCache],
  );
};

export default SubstanceAutocomplete;
