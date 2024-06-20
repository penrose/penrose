import { CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { printTree } from "@lezer-unofficial/printer";
import { SyntaxNode } from "@lezer/common";
import { constrDict } from "@penrose/core";
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

const getConstraints = () => {
  return Object.entries(constrDict).flatMap(([key, value]) => [
    {
      label: key,
      type: "function ",
      info: "description" in value ? value.description : "",
    },
  ]);
};

const typeNames = [
  "scalar",
  "int",
  "bool",
  "string",
  "path",
  "color",
  "file",
  "style",
  "shape",
  "vec2",
  "vec3",
  "vec4",
  "mat2x2",
  "mat3x3",
  "mat4x4",
  "function",
  "objective",
  "constraint",
].map((name) => ({
  label: `${name} `,
  type: "type",
}));

const headerOptions = ["forall", "collect", "layout"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
}));

const selectorKws = ["override", "layer", "encourage", "ensure"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
}));

const collectKws = ["encourage", "ensure"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
}));

const goToParentX = (parentNode: SyntaxNode, x: number) => {
  let i = 0;
  let nextParent: SyntaxNode | null = parentNode;
  while (i < x) {
    nextParent = nextParent.parent;
    i++;
    if (nextParent == null) {
      return null;
    }
  }
  return nextParent;
};

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

      if (parentNode == null) {
        return null;
      }

      /*
       * Shape property auto complete. Properties nested as identifier
       * inside PropName inside PropertyDecl inside ShapeDecl
       */
      if (
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
        // Go up to header
        parentNode.parent != null &&
        parentNode.parent.name === "Header"
      ) {
        return {
          from: word.from,
          options: headerOptions,
        };
      }
      if (parentNode != null) console.log(goToParentX(parentNode, 5));

      // Constraint completion
      // Path: StyVar, Var, Path, Expr, ObjConstrBody, Constraint/Objective
      // (5+parentNode)
      const upSix = goToParentX(parentNode, 5);
      if (
        upSix != null &&
        (upSix.name === "Objective" || upSix.name === "Constraint")
      ) {
        return {
          from: word.from,
          options: getConstraints(),
        };
      }

      // Namespace, Selector, Collect block completion
      // Path: StyVar, Var, Path, Assign, Statement, Block, HeaderBlock
      const upSeven = goToParentX(parentNode, 6);
      if (
        upSeven != null &&
        // Go down into Header
        upSeven.firstChild != null &&
        // Go down into Namespace
        upSeven.firstChild.firstChild != null
      ) {
        // Suggest type names inside Namespace
        if (upSeven.firstChild.firstChild.name === "Namespace") {
          return {
            from: word.from,
            options: typeNames,
          };
          // Suggest typenames and kws inside Selector
        } else if (upSeven.firstChild.firstChild.name === "Selector") {
          return {
            from: word.from,
            options: typeNames.concat(selectorKws),
          };
          // Suggest typenames and kws inside Collect
        } else if (upSeven.firstChild.firstChild.name === "Collector") {
          return {
            from: word.from,
            options: typeNames.concat(collectKws),
          };
        }
      }

      return null;
    },
    [domainCache, shapeDefns],
  );
};

export default StyleAutocomplete;
