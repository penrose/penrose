import { SyntaxNode, TreeCursor } from "@lezer/common";
import {
  ShapeType,
  makeCanvas,
  sampleShape,
  shapeTypes,
  simpleContext,
} from "@penrose/core";
import { ShapeDefinitions } from "../types";

export const extractText = (progText: string, to: number, from: number) => {
  return progText.slice(from, to);
};

/**
 * Retrieves defintions for all shapes and writes their properties to a
 * hashmap object with shapeName as the top level key with the value being
 * a hashmap of properties that belong to the shape as keys with type as value
 */
export const getShapeDefs = (): ShapeDefinitions => {
  const shapeProps = {} as ShapeDefinitions;
  const size = 311; // placeholder, this value doesn't matter

  for (const shapeName of shapeTypes) {
    const shapeSample = sampleShape(
      shapeName as ShapeType,
      simpleContext("ShapeProps dummy"),
      makeCanvas(size, size),
    );

    shapeProps[shapeName] = Object.fromEntries(
      Object.entries(shapeSample).map(([key, value]) => [key, value.tag]),
    );
  }

  return shapeProps;
};
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

export const goUpToTarget = (node: SyntaxNode, targetName: string) => {
  let nextParent: SyntaxNode | null = node;
  while (nextParent != null) {
    if (nextParent.name === targetName) return nextParent;
    nextParent = nextParent.parent;
  }
  return null;
};

export const goDownToTarget = (node: SyntaxNode, targetName: string) => {
  let nextChild: SyntaxNode | null = node;
  while (nextChild != null) {
    if (nextChild.name === targetName) return nextChild;
    nextChild = nextChild.firstChild;
  }
  return null;
};

/*
 * Calling .firstChild() will move cursor in place
 */
export const traverseCursorDown = (cursor: TreeCursor, target: string) => {
  while (cursor.firstChild()) {
    if (cursor.name == target) return true;
  }
  return false;
};

/*
 * Calling .parent() will move cursor in place
 */
export const traverseCursorUp = (cursor: TreeCursor, target: string) => {
  while (cursor.parent()) {
    if (cursor.name == target) return true;
  }
  return false;
};
