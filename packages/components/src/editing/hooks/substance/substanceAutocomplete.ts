import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { SyntaxNode } from "@lezer/common";
import { useCallback } from "react";
import { DomainCache } from "../../../editing/types";

const keywordOptions = ["Let", "AutoLabel", "Label", "NoLabel"].map((kw) => ({
  label: kw,
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

const predTypeOptions = (domainCache: DomainCache) => {
  const typeOptions = domainCache.typeNames.map((type) => ({
    label: type,
    type: "variable",
    detail: "type",
  }));

  const predicateOptions = domainCache.predNames.map((type) => ({
    label: type,
    type: "variable",
    detail: "predicate",
  }));

  return typeOptions.concat(predicateOptions);
};

const fnConsOptions = (domainCache: DomainCache) => {
  const fnOptions = domainCache.fnNames.map((type) => ({
    label: type,
    type: "variable",
    detail: "function",
  }));

  const consOptions = domainCache.consNames.map((type) => ({
    label: type,
    type: "variable",
    detail: "constructor",
  }));

  return fnOptions.concat(consOptions);
};

const SubstanceAutocomplete = (domainCache: DomainCache) => {
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

      //   console.log(parentNode, leftSib, wholeTree.toString());

      // not sure what this does, stolen from autocomplete example
      if (word == null || (word.from === word.to && !context.explicit)) {
        return null;
      }

      /*
        Autocomplete keyword only if there's nothing to the left of it
        */
      if (leftSib === null && InsideStatement(parentNode)) {
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
          options: [{ label: "All", type: "keyword" }],
        };
      } else if (leftSib !== null && leftSib.name === "Range") {
        return {
          from: word.from,
          options: [{ label: "where", type: "keyword" }],
        };
      }
      // for suggestor, will suggest in correct spaces in all but labeling statements
      else if (
        InsideStatement(parentNode) &&
        leftSib !== null &&
        // To suggest after TypeApp
        ((leftSib.name === "Identifier" &&
          leftSib.prevSibling != null &&
          leftSib.prevSibling.type.name === "Identifier") ||
          // To suggest after PredicateApp and Fn_ConsApp
          leftSib.name === "ArgList")
      ) {
        return {
          from: word.from,
          options: [{ label: "for", type: "keyword" }],
        };
      }

      // Suggest function and constructor names
      if (leftSib !== null && leftSib.name === "Assignment") {
        return {
          from: word.from,
          options: fnConsOptions(domainCache),
        };
      }

      return null;
    },
    [domainCache],
  );
};

export default SubstanceAutocomplete;
