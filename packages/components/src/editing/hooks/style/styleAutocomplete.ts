import { Completion, CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { printTree } from "@lezer-unofficial/printer";
import { useCallback } from "react";
import { DomainCache, ShapeDefinitions } from "../../types";
import { extractText } from "../hooksUtils";
import {
  anonExprKws,
  exprOptions,
  getConstraints,
  getPredOptions,
  getShapeNames,
  getShapeProps,
  getTypeOptions,
  goToParentX,
  headerOptions,
  statementKws,
  typeNames,
} from "./styleAutocompleteUtils";

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

      console.log(
        printTree(wholeTree, context.state.doc.toString()),
        nodeBefore,
        parentNode,
      );
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
      const upThree = goToParentX(parentNode, 2);
      if (
        upThree != null &&
        upThree.name === "ShapeDecl" &&
        upThree.firstChild !== null
      ) {
        // Get shape name
        let shapeNameNode = upThree.firstChild;
        let shapeName = extractText(
          context.state.doc.toString(),
          shapeNameNode.to,
          shapeNameNode.from,
        );
        // We allow arbitrary shape names, so check it actually exists
        if (shapeDefns[shapeName]) {
          // We shortciruit to avoid triggering other autocomplete fns
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
        completionOpts = completionOpts.concat(headerOptions);
      }

      // Expr completion
      // StyVar -> Var -> Path -> Expr
      const upFour = goToParentX(parentNode, 3);
      if (upFour != null && upFour.name === "Expr") {
        completionOpts = completionOpts.concat(exprOptions);
      }

      // AssignExpr completion
      // StyVar -> Var -> Path -> Expr -> AssignExpr
      const upFive = goToParentX(parentNode, 4);
      if (upFive != null && upFive.name === "AssignExpr") {
        completionOpts = completionOpts
          .concat(anonExprKws)
          .concat(getShapeNames(shapeDefns));
      }

      // Constraint completion
      // StyVar -> Var -> Path -> Expr -> ObjConstrBody -> Constraint/Objective
      // (5+parentNode)
      const upSix = goToParentX(parentNode, 5);
      if (
        upSix != null &&
        (upSix.name === "Objective" || upSix.name === "Constraint")
      ) {
        completionOpts = completionOpts.concat(getConstraints());
      }

      // Namespace, Selector, Collect block completion
      // StyVar -> Var -> Path -> Assign -> Statement -> Block -> HeaderBlock
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
        parentNode.prevSibling != null &&
        parentNode.prevSibling.name === "Decl"
      ) {
        completionOpts = completionOpts.concat([
          { label: "into ", type: "keyword" },
        ]);
      }

      // Suggest pred names in where header
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
