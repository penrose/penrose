import { SyntaxNode } from "@lezer/common";
import { compDict, constrDict } from "@penrose/core";
import { DomainCache, ShapeDefinitions, ShapeProperties } from "../../types";
import { extractText } from "../hooksUtils";

export const getShapeProps = (shapeProps: ShapeProperties) => {
  return Object.entries(shapeProps).flatMap(([key, value]) => [
    {
      label: key,
      type: "property",
      info: value,
    },
  ]);
};

export const getConstraints = () => {
  return Object.entries(constrDict).flatMap(([key, value]) => [
    {
      label: key,
      type: "function ",
      info: "description" in value ? value.description : "",
    },
  ]);
};

export const getComputationFns = () => {
  return Object.entries(compDict).flatMap(([key, value]) => [
    {
      label: value.name,
      type: "function",
      info: "description" in value ? value.description : "",
    },
  ]);
};

export const getShapeNames = (shapeDefns: ShapeDefinitions) => {
  return Object.entries(shapeDefns).flatMap(([key, value]) => [
    {
      label: `${key} `,
      // idk what the property type here should be
      type: "class",
      info: "",
    },
  ]);
};

export const getTypeOptions = (domainCache: DomainCache) => {
  return domainCache.typeNames.map((type) => ({
    label: `${type} `,
    type: "type",
    info: "",
  }));
};

export const getPredOptions = (domainCache: DomainCache) => {
  return domainCache.predNames.map((pred) => ({
    label: `${pred} `,
    type: "type",
    info: "",
  }));
};

/*
 * Create hashmap structure with namespace identifiers as keys and
 * declared variables array as value
 */
export const getNamespaceDict = (topNode: SyntaxNode, styleProg: string) => {
  // topNode is type input, walk down into items
  const itemsNode = topNode.getChild("Items");
  if (itemsNode === null) return {};
  const itemNodes = itemsNode.getChildren("Item");
  let namespaceCache = {} as { [Key: string]: string[] };

  itemNodes.forEach((node: SyntaxNode) => {
    let nodeCursor = node.cursor();
    // move to "HeaderBlock"
    nodeCursor.firstChild();
    if (nodeCursor.name !== "HeaderBlock") return;
    // move to Header
    nodeCursor.firstChild();
    // Move to Namespace
    nodeCursor.firstChild();
    // Node cursor throws a typescript error on checking the names successively
    // @ts-ignore
    if (nodeCursor.name !== "Namespace") return;
    // Move to Identifier
    nodeCursor.firstChild();
    let blockName = extractText(styleProg, nodeCursor.to, nodeCursor.from);
    if (blockName === "canvas") return;
    let varNames = [] as string[];
    // Move to Namespace
    nodeCursor.parent();
    // Move to Header
    nodeCursor.parent();
    // Move to Block
    nodeCursor.nextSibling();
    // Get children
    let statements = nodeCursor.node.getChildren("Statement");
    statements.forEach((stNode: SyntaxNode) => {
      let stNodeCursor = stNode.cursor();
      stNodeCursor.firstChild();
      if (stNodeCursor.name !== "Assign") return;
      // Into either Type or Path
      stNodeCursor.firstChild();
      // @ts-ignore
      if (stNodeCursor.name === "Type") {
        stNodeCursor.nextSibling();
      }
      // Should now be in Path
      // Move into Variable
      stNodeCursor.firstChild();
      // Move into Styvar
      stNodeCursor.firstChild();
      // Move into Identifier
      stNodeCursor.firstChild();
      varNames.push(extractText(styleProg, stNodeCursor.to, stNodeCursor.from));
    });
    namespaceCache[blockName] = varNames;
  });

  return namespaceCache;
};

export const getNamespaces = (topNode: SyntaxNode, styleProg: string) => {
  let namespaceDict = getNamespaceDict(topNode, styleProg);

  return Object.entries(namespaceDict).map(([key, value]) => ({
    label: key,
    type: "namespace",
    info: "",
  }));
};

export const getNamespaceProps = (
  topNode: SyntaxNode,
  styleProg: string,
  namespace: string,
) => {
  let namespaceDict = getNamespaceDict(topNode, styleProg);
  if (namespace in namespaceDict) {
    return namespaceDict[namespace].map((prop) => ({
      label: prop,
      type: "property",
      info: "",
    }));
  }
  return [];
};

export const exprKws = ["true", "false", "listof"].map((name) => ({
  label: `${name}`,
  type: "keyword",
  info: "",
}));

export const typeNames = [
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

export const headerOptions = ["forall", "collect", "layout"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
  info: "",
}));

export const statementKws = ["delete", "override"].map((kw) => ({
  label: `${kw} `,
  type: "keyword",
  info: "",
}));

export const anonExprKws = ["override", "layer", "encourage", "ensure"].map(
  (kw) => ({
    label: `${kw} `,
    type: "keyword",
    info: "",
  }),
);

export const goToParentX = (node: SyntaxNode, x: number) => {
  let i = 0;
  let nextParent: SyntaxNode | null = node;
  while (i < x) {
    nextParent = nextParent.parent;
    i++;
    if (nextParent == null) {
      return null;
    }
  }
  return nextParent;
};

export const goToChildX = (node: SyntaxNode, x: number) => {
  let i = 0;
  let nextChild: SyntaxNode | null = node;
  while (i < x) {
    nextChild = nextChild.firstChild;
    i++;
    if (nextChild == null) {
      return null;
    }
  }
  return nextChild;
};
