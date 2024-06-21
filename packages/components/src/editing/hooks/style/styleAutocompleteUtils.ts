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
export const getNamespaces = (topNode: SyntaxNode, styleProg: string) => {
  // topNode is type input, walk down into items
  const itemsNode = topNode.getChild("Items");
  if (itemsNode === null) return [];
  const itemNodes = itemsNode.getChildren("Item");
  let namespaceCache = {} as { string: [string] };

  itemNodes.forEach((node: SyntaxNode) => {
    console.log(node);
    let nodeCursor = node.cursor();
    // move to "HeaderBlock"
    nodeCursor.firstChild();
    if (nodeCursor.name !== "HeaderBlock") return;
    console.log("in headerblock");
    // move to Header
    nodeCursor.firstChild();
    if (nodeCursor.name !== "Header") return;
    // Move to Namespace
    nodeCursor.firstChild();

    if (nodeCursor.name !== "Namespace") return;
    // Move to Identifier
    nodeCursor.firstChild();

    if (nodeCursor.name === "Identifier") {
      let blockName = extractText(styleProg, nodeCursor.to, nodeCursor.from);
      if (blockName === "canvas") return;
      let varNames = [];
    }
  });

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

export const goToParentX = (parentNode: SyntaxNode, x: number) => {
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
