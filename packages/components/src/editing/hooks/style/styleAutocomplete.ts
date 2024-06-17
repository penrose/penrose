import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { useCallback } from "react";

const StyleAutocomplete = () => {
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

      //   console.log(wholeTree.toString(), leftSib, parent);
      console.log(wholeTree.toString());

      // not sure what this does, stolen from autocomplete example
      if (word == null || (word.from === word.to && !context.explicit)) {
        return null;
      }
      return null;
    },
    [],
    // need to specify, otherwise domainCache won't update correctly
  );
};

export default StyleAutocomplete;
