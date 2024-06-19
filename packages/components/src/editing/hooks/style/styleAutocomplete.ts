import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { printTree } from "@lezer-unofficial/printer";
import { useCallback } from "react";
import { DomainCache, ShapeDefinitions, ShapeProperties } from "../../types";
import { extractText } from "../hooksUtils";

const getShapeProps = (shapeProps: ShapeProperties) => {
  return Object.entries(shapeProps).flatMap(([key, value]) => [
    {
      label: key,
      type: "property",
      detail: value,
    },
  ]);
};

const headerOptions = ["forall", "collect", "layout"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
}));

const StyleAutocomplete = (
  domainCache: DomainCache,
  shapeDefns: ShapeDefinitions,
) => {
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
      console.log(
        printTree(wholeTree, context.state.doc.toString()),
        nodeBefore,
        parentNode,
      );
      // console.log(wholeTree.toString(), leftSib, parent);
      // console.log(wholeTree.toString());

      // not sure what this does, stolen from autocomplete example
      if (word == null || (word.from === word.to && !context.explicit)) {
        return null;
      }

      /*
       * Shape property auto complete. Properties nested as identifier
       * inside PropName inside PropertyDecl inside ShapeDecl
       */
      if (
        // Inside PropName
        parentNode != null &&
        // Inside PropertyDecl
        parentNode.parent != null &&
        // Inside ShapeDecl
        parentNode.parent.parent != null &&
        parentNode.parent.parent.name === "ShapeDecl" &&
        // There is a ShapeName
        parentNode.parent.parent.firstChild !== null
      ) {
        // Get shape name
        let shapeNameNode = parentNode.parent.parent.firstChild;
        let shapeName = extractText(
          context.state.doc.toString(),
          shapeNameNode.to,
          shapeNameNode.from,
        );
        // We allow arbitrary shape names, so check it actually exists
        if (shapeDefns[shapeName]) {
          return {
            from: word.from,
            options: getShapeProps(shapeDefns[shapeName]),
          };
        }
      }

      // Top level kw completion (forall, collect, layout)
      if (
        // Go up to namespace
        parentNode != null &&
        // Go up to header
        parentNode.parent != null &&
        parentNode.parent.name === "Header"
      ) {
        return {
          from: word.from,
          options: headerOptions,
        };
      }

      return null;
    },
    [domainCache, shapeDefns],
  );
};

export default StyleAutocomplete;
