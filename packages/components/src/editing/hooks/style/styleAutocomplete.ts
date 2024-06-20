import { Completion, CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { printTree } from "@lezer-unofficial/printer";
import { SyntaxNode } from "@lezer/common";
import { compDict, constrDict } from "@penrose/core";
import { useCallback } from "react";
import { DomainCache, ShapeDefinitions, ShapeProperties } from "../../types";
import { extractText } from "../hooksUtils";

const getShapeProps = (shapeProps: ShapeProperties) => {
  return Object.entries(shapeProps).flatMap(([key, value]) => [
    {
      label: key,
      type: "property",
      info: value,
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

const getComputationFns = () => {
  return Object.entries(compDict).flatMap(([key, value]) => [
    {
      label: value.name,
      type: "function",
      info: "description" in value ? value.description : "",
    },
  ]);
};

const exprOptions = ["true", "false", "listof"]
  .map((name) => ({
    label: `${name}`,
    type: "keyword",
    info: "",
  }))
  .concat(getComputationFns());

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
  info: "",
}));

const headerOptions = ["forall", "collect", "layout"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
  info: "",
}));

const statementKws = ["delete", "override"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
  info: "",
}));

const anonExprKws = ["override", "layer", "encourage", "ensure"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
  info: "",
}));

const getShapeNames = (shapeDefns: ShapeDefinitions) => {
  return Object.entries(shapeDefns).flatMap(([key, value]) => [
    {
      label: `${key} `,
      // idk what the property type here should be
      type: "class",
      info: "",
    },
  ]);
};

const getTypeOptions = (domainCache: DomainCache) => {
  return domainCache.typeNames.map((type) => ({
    label: `${type} `,
    type: "type",
    info: "",
  }));
};

const getPredOptions = (domainCache: DomainCache) => {
  return domainCache.predNames.map((pred) => ({
    label: `${pred} `,
    type: "type",
    info: "",
  }));
};

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
      let word = context.matchBefore(/\w*/);
      let wholeTree = syntaxTree(context.state).topNode;
      //   console.log(domainCache);
      // In erorr state, error node wraps the current node
      // if (parentNode != null && parentNode.type.isError) {
      //   leftSib = parentNode.prevSibling;
      //   parentNode = parentNode.parent;
      // }
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

      let completionOpts = [] as Completion[];

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
          completionOpts = completionOpts.concat(
            getShapeProps(shapeDefns[shapeName]),
          );
        }
      }

      // Top level kw completion (forall, collect, layout)
      if (
        // Go up to header
        parentNode.parent != null &&
        parentNode.parent.name === "Header"
      ) {
        completionOpts = completionOpts.concat(headerOptions);
      }

      // Expr completion
      const upFour = goToParentX(parentNode, 3);
      if (upFour != null && upFour.name === "Expr") {
        completionOpts = completionOpts.concat(exprOptions);
      }

      // AssignExpr completion
      const upFive = goToParentX(parentNode, 4);
      if (upFive != null && upFive.name === "AssignExpr") {
        completionOpts = completionOpts
          .concat(anonExprKws)
          .concat(getShapeNames(shapeDefns));
      }

      // Constraint completion
      // Path: StyVar, Var, Path, Expr, ObjConstrBody, Constraint/Objective
      // (5+parentNode)
      const upSix = goToParentX(parentNode, 5);
      if (
        upSix != null &&
        (upSix.name === "Objective" || upSix.name === "Constraint")
      ) {
        completionOpts = completionOpts.concat(getConstraints());
      }

      // Namespace, Selector, Collect block completion
      // Path: StyVar, Var, Path, Assign, Statement, Block, HeaderBlock
      // const upSeven = goToParentX(parentNode, 6);
      if (upSix != null && upSix.name === "Block") {
        completionOpts = completionOpts
          .concat(typeNames)
          .concat(anonExprKws)
          .concat(statementKws)
          .concat(getShapeNames(shapeDefns));
      }

      // Suggest type names
      if (
        parentNode != null &&
        parentNode.name === "SelType" &&
        nodeBefore.name === "Identifier"
      ) {
        completionOpts = completionOpts.concat(getTypeOptions(domainCache));
      }

      // Collect Header autocomplete
      // Collect suggest repeatable
      if (
        // SelType -> Decl
        parentNode.parent != null &&
        parentNode.parent.prevSibling != null &&
        parentNode.parent.prevSibling.name === "collect"
      ) {
        completionOpts = completionOpts.concat([
          { label: "repeatable", type: "keyword" },
        ]);
      }

      // Collect suggest into
      if (
        // Into error node
        parentNode != null &&
        parentNode != null &&
        parentNode.prevSibling != null &&
        parentNode.prevSibling.name === "Decl"
      ) {
        completionOpts = completionOpts.concat([
          { label: "into ", type: "keyword" },
        ]);
      }

      // Suggest pred names in where
      // StyVar -> Variable -> Bind -> Relation
      if (upFour != null && upFour.name === "Relation") {
        completionOpts = completionOpts.concat(getPredOptions(domainCache));
      }

      if (completionOpts.length > 0) {
        return {
          from: word.from,
          options: completionOpts,
        };
      }
      return null;
    },
    [domainCache, shapeDefns],
  );
};

export default StyleAutocomplete;
