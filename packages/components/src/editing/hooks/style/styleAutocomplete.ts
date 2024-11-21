import { Completion, CompletionContext } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { useCallback } from "react";
import { DomainCache, ShapeDefinitions } from "../../types";
import {
  extractText,
  goDownToTarget,
  goToParentX,
  goUpToTarget,
} from "../hooksUtils";
import {
  anonExprKws,
  collectorHeaderOptions,
  exprKws,
  getComputationFns,
  getConstraints_Objectives,
  getNamespaceProps,
  getNamespaces,
  getPredOptions,
  getShapeNames,
  getShapeProps,
  getStageNameOpts,
  getTypeOptions,
  headerOptions,
  selectorHeaderOptions,
  statementKws,
  typeNames,
} from "./styleAutocompleteUtils";

export const createStyleAutocomplete = (
  domainCache: DomainCache,
  shapeDefns: ShapeDefinitions,
) => {
  return async (context: CompletionContext) => {
    let nodeBefore = syntaxTree(context.state).resolveInner(context.pos, -1);
    let parentNode = nodeBefore.parent;
    let word = context.matchBefore(/\w*/);

    if (word == null) {
      return null;
    }

    // Suggest user defined namespace properties
    if (
      parentNode?.name === "Path" &&
      // Case 1) User typed .
      (nodeBefore.name === "." ||
        // Case 2) User typed .[something]
        (nodeBefore.name === "Identifier" &&
          nodeBefore?.prevSibling?.name === "."))
    ) {
      // Grab namespace identifier, here parentNode is Path
      let id = goDownToTarget(parentNode, "Identifier");
      if (id != null) {
        const topNode = syntaxTree(context.state).topNode;
        const styleProg = context.state.doc.toString();
        const namespace = extractText(styleProg, id.to, id.from);

        // We shortciruit to avoid triggering other autocomplete fns
        return {
          from: word.from,
          options: getNamespaceProps(topNode, styleProg, namespace),
        };
      }
    }

    /*
     * Order matters here, this check prevents completion of namespace
     * properties in the . case
     * This check is necessary to avoid triggering completions on ;
     */
    if (word.from == word.to && !context.explicit) {
      return null;
    }

    /*
     * We track completionOpts and return at the end for the cases where
     * multiple sets of completion items should trigger.
     */
    let completionOpts = [] as Completion[];
    /*
     * Shape property auto complete.
     * Triggers if cursor is inside both PropName and ShapeDecl
     * Check ShapeDecl exists so we can get shapeNameNode
     */
    const searchForPropName = goUpToTarget(nodeBefore, "PropName");
    const searchForShapeDecl = goUpToTarget(nodeBefore, "ShapeDecl");
    if (searchForPropName != null && searchForShapeDecl != null) {
      // ShapeDecl -> ShapeName -> Identifier
      let shapeNameNode = goDownToTarget(searchForShapeDecl, "Identifier");

      if (shapeNameNode != null) {
        let shapeName = extractText(
          context.state.doc.toString(),
          shapeNameNode.to,
          shapeNameNode.from,
        );

        // We allow arbitrary shape names in parse, so check it actually exists
        if (shapeDefns[shapeName]) {
          // We shortciruit to avoid triggering other autocomplete fns
          return {
            from: word.from,
            options: getShapeProps(shapeDefns[shapeName]),
          };
        }
      }
    }

    /*
     * Collector and Selector header completion
     * Language ambiguity issue: start of keywords like "where" and "which"
     * in the header are assumed to be the start of a new namespace block
     * We check for this error state and shortcircuit
     */
    let itemNode = goUpToTarget(nodeBefore, "Item");
    // Check if previous item exists and is a HeaderBlock
    if (itemNode?.prevSibling?.firstChild?.name === "HeaderBlock") {
      let prevHeaderBlock = itemNode.prevSibling.firstChild;
      // Checks for error state in the position of block (in prev HeaderBlock)
      if (prevHeaderBlock.lastChild?.type.isError) {
        if (goDownToTarget(prevHeaderBlock, "Selector")) {
          return {
            from: word.from,
            options: selectorHeaderOptions,
          };
        }
        if (goDownToTarget(prevHeaderBlock, "Collector")) {
          return {
            from: word.from,
            options: collectorHeaderOptions,
          };
        }
      }
    }

    // Stage Name autocomplete
    if (
      goUpToTarget(nodeBefore, "StageSpecifier") &&
      // Avoid suggesting where "in"/"except" go
      parentNode?.prevSibling != null
    ) {
      const topNode = syntaxTree(context.state).topNode;
      const styleProg = context.state.doc.toString();

      return {
        from: word.from,
        options: getStageNameOpts(topNode, styleProg),
      };
    }

    // Top level kw completion (forall, collect, layout)
    if (parentNode?.parent?.name === "Header") {
      completionOpts = completionOpts.concat(headerOptions);
    }

    /*
     * Expr completion
     * Check nodeBefore name to avoid triggering on Numbers
     * parser guesses Identifier before term completed
     */
    if (
      goUpToTarget(nodeBefore, "Expr") != null &&
      nodeBefore.name === "Identifier"
    ) {
      const topNode = syntaxTree(context.state).topNode;
      const styleProg = context.state.doc.toString();
      completionOpts = completionOpts
        .concat(exprKws)
        .concat(getComputationFns())
        .concat(getNamespaces(topNode, styleProg));
    }

    // AssignExpr completion
    if (
      goUpToTarget(nodeBefore, "AssignExpr") != null &&
      nodeBefore.name === "Identifier"
    ) {
      completionOpts = completionOpts
        .concat(anonExprKws)
        .concat(getShapeNames(shapeDefns));
    }

    // Constraint + Objective fn completion (following "encourage" and "ensure")
    if (goUpToTarget(nodeBefore, "ObjConstrBody") != null) {
      completionOpts = completionOpts.concat(getConstraints_Objectives());
    }

    /*
     * Namespace, Selector, Collect block completion
     * StyVar -> Var -> Path -> Assign -> Statement -> Block
     * Doesn't use goUpToTarget due to deep nesting possible in Block
     */
    const upSix = goToParentX(nodeBefore, 6);
    if (upSix?.name === "Block") {
      completionOpts = completionOpts
        .concat(typeNames)
        .concat(anonExprKws)
        .concat(statementKws)
        .concat(getShapeNames(shapeDefns));
    }

    // Suggest domain-defined type names
    if (parentNode?.name === "SelType" && nodeBefore.name === "Identifier") {
      completionOpts = completionOpts.concat(getTypeOptions(domainCache));
    }

    // Selector Header autocomplete
    // Selector suggest repeatable
    const searchForDeclPattern = goUpToTarget(nodeBefore, "DeclPattern");
    if (
      searchForDeclPattern != null &&
      // Avoid suggesting in cases like forall Real a; r
      searchForDeclPattern.prevSibling == null
    ) {
      const searchForDeclList = goUpToTarget(searchForDeclPattern, "DeclList");
      // Check the node we're at comes right after "forall"
      if (searchForDeclList?.prevSibling?.name === "forall") {
        completionOpts = completionOpts.concat([
          { label: "repeatable", type: "keyword" },
        ]);
      }
    }

    // Collect Header autocomplete
    // Collect suggest repeatable
    if (parentNode?.parent?.prevSibling?.name === "collect") {
      completionOpts = completionOpts.concat([
        { label: "repeatable", type: "keyword" },
      ]);
    }

    // Collect suggest into
    if (
      // Parser wraps start of into in an error node, hence why we take parent
      parentNode?.prevSibling?.name === "Decl"
    ) {
      completionOpts = completionOpts.concat([
        { label: "into ", type: "keyword" },
      ]);
    }

    /*
     * Parser defaults to Bind in partial parse state. Null checks to ensure
     * this does not trigger inside actual Bind and Field expressions
     */
    if (
      goUpToTarget(nodeBefore, "Relation") != null &&
      goUpToTarget(nodeBefore, "RelExpr") == null &&
      goUpToTarget(nodeBefore, "Field") == null
    ) {
      completionOpts = completionOpts.concat(getPredOptions(domainCache));
    }

    if (completionOpts.length > 0) {
      return {
        from: word.from,
        options: completionOpts,
      };
    }
    return null;
  };
};

const StyleAutocomplete = (
  domainCache: DomainCache,
  shapeDefns: ShapeDefinitions,
) => {
  return useCallback(createStyleAutocomplete(domainCache, shapeDefns), [
    domainCache,
    shapeDefns,
  ]);
};

export default StyleAutocomplete;
