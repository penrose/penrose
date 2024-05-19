import { A, C, Identifier } from "../types/ast.js";
import {
  BadStylePathToValueObject,
  BlockAssignment,
  BlockInfo,
  EmptyStylePath,
  FieldDict,
  StySubst,
  StylePath,
  StylePathToObject,
  StylePathToScope,
  StylePathToShapeObject,
  SubstanceObject,
} from "../types/styleSemantics.js";

import im from "immutable";
import { StyleError } from "../types/errors.js";
import { Path } from "../types/style.js";
import { Result, err, ok } from "./Error.js";
import { subObjectToUniqueName } from "./Util.js";

type StylePathResolverResult<T> = {
  resolvedPath: StylePath<T>;
  remaining: Identifier<T>[];
};

const pathRefersToShapeObject = <T>(
  assignment: BlockAssignment,
  path: StylePathToScope<T> | StylePathToObject<T>,
): path is StylePathToShapeObject<T> => {
  if (path.tag !== "Object") {
    return false;
  }

  const findPathContents = (path: StylePathToScope<T>): FieldDict => {
    if (path.tag === "Local") {
      return assignment.locals;
    } else if (path.tag === "Namespace") {
      return assignment.globals.get(path.name) ?? im.Map();
    } else {
      return assignment.substances.get(path.substanceName) ?? im.Map();
    }
  };

  const { name, parent } = path;

  // `path` refers to an object, either a shape object or a value object
  // in either cases, `parent` is either a scope or a shape object, since a value object cannot be a parent.

  if (parent.tag !== "Object") {
    const source = findPathContents(parent).get(name);
    if (source === undefined) return false;
    else return source.tag === "ShapeSource";
  }

  // parent is not a scope and hence is a shape object
  // children of the parent, then, cannot be a shape object.
  return false;
};

export const resolveLhsStylePath = (
  { block, subst }: BlockInfo,
  assignment: BlockAssignment,
  path: Path<C>,
): Result<StylePath<C>, StyleError> => {
  const init: EmptyStylePath<C> = {
    tag: "Empty",
    nodeType: "Style",
    start: path.start,
    end: path.start,
  };

  const firstPart: Identifier<C> =
    path.name.tag === "StyVar"
      ? path.name.contents
      : {
          tag: "Identifier",
          value: `\`${path.name.contents.value}\``,
          start: path.name.start,
          end: path.name.end,
          nodeType: "Style",
          type: "value",
        };

  const rest = path.members;

  const parts = [firstPart, ...rest];

  const helper = (
    result: StylePathResolverResult<C>,
  ): Result<StylePath<C>, StyleError> => {
    if (result.remaining.length === 0) {
      return ok(result.resolvedPath);
    } else {
      return resolveLhsStylePathPart(
        { block, subst },
        assignment,
        result.resolvedPath,
        result.remaining,
      ).andThen(helper);
    }
  };

  return helper({ resolvedPath: init, remaining: parts });
};

const findFromSubst = (
  subst: StySubst,
  key: string,
): SubstanceObject | undefined => {
  if (subst.tag === "StySubSubst") {
    return subst.contents[key];
  } else {
    return subst.groupby[key];
  }
};

const resolveLhsStylePathPart = (
  { block, subst }: BlockInfo,
  assignment: BlockAssignment,
  curr: StylePath<C>,
  parts: Identifier<C>[],
): Result<StylePathResolverResult<C>, StyleError> => {
  const [next, ...rest] = parts;
  if (curr.tag === "Empty") {
    const subObj = findFromSubst(subst, next.value);
    if (subObj !== undefined) {
      return ok({
        resolvedPath: {
          tag: "Substance",
          substanceName: subObjectToUniqueName(subObj),
          styleName: next.value,
          nodeType: "Style",
          start: curr.start,
          end: next.end,
        },
        remaining: rest,
      });
    } else if (assignment.globals.has(next.value)) {
      return ok({
        resolvedPath: {
          tag: "Namespace",
          name: next.value,
          nodeType: "Style",
          start: curr.start,
          end: next.end,
        },
        remaining: rest,
      });
    } else {
      return ok({
        resolvedPath: {
          tag: "Local",
          nodeType: "Style",
          start: curr.start,
          end: curr.end,
        },
        // not `rest` here, since we only know that `curr` is in local,
        // we have not resolved `curr` yet.
        remaining: parts,
      });
    }
  } else if (
    // these are scopes
    curr.tag === "Local" ||
    curr.tag === "Namespace" ||
    curr.tag === "Substance"
  ) {
    // for LHS,
    // we don't need to check if `next` actually exists in the scope indicated by `curr`
    // because we may be declaring it for the first time.

    // for RHS we definitely need to check this.
    return ok({
      resolvedPath: {
        tag: "Object",
        parent: curr,
        name: next.value,
        nodeType: "Style",
        start: curr.start,
        end: next.end,
      },
      remaining: rest,
    });
  } else {
    // check if `curr` refers to a shape
    // if so, then `next` must be a property of an already-defined shape (which must already be in `assignment`), so that is fine
    // if not, then fail since we cannot index into a property of a non-shape
    if (pathRefersToShapeObject(assignment, curr)) {
      return ok({
        resolvedPath: {
          tag: "Object",
          parent: curr,
          name: next.value,
          nodeType: "Style",
          start: curr.start,
          end: next.end,
        },
        remaining: rest,
      });
    } else {
      return err({
        tag: "InvalidLhsPathError",
        path: {
          tag: "Object",
          parent: curr,
          name: next.value,
          nodeType: "Style",
          start: curr.start,
          end: next.end,
        },
      });
    }
  }
};

export const prettyStylePath = (
  p: StylePath<A> | BadStylePathToValueObject<A>,
): string => {
  if (p.tag === "Empty") {
    return "";
  } else if (p.tag === "Local") {
    return "";
  } else if (p.tag === "Namespace") {
    return p.name;
  } else if (p.tag === "Substance") {
    return p.substanceName;
  } else {
    const ppParent = prettyStylePath(p.parent);
    if (ppParent === "") {
      return p.name;
    } else {
      return `${ppParent}.${p.name}`;
    }
  }
};
