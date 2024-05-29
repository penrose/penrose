import { isConcrete } from "../engine/EngineUtils.js";
import {
  A,
  ASTNode,
  C,
  Identifier,
  NodeType,
  SourceLoc,
} from "../types/ast.js";
import { StyleError } from "../types/errors.js";
import {
  Path,
  ResolvedStylePath,
  StylePathToNamespaceScope,
  StylePathToObject,
  StylePathToScope,
  StylePathToSubstanceScope,
} from "../types/style.js";
import {
  BlockAssignment,
  BlockInfo,
  StySubst,
  SubstanceObject,
} from "../types/styleSemantics.js";
import { Result, err, ok } from "../utils/Error.js";
import { subObjectToUniqueName } from "./Util.js";
const loc = (
  x: ASTNode<A>,
): { nodeType: NodeType; start?: SourceLoc; end?: SourceLoc } => {
  if (isConcrete(x)) {
    return { start: x.start, end: x.end, nodeType: x.nodeType };
  } else {
    return { nodeType: x.nodeType };
  }
};

const locs = (
  x: ASTNode<A>,
  y: ASTNode<A>,
): { nodeType: NodeType; start?: SourceLoc; end?: SourceLoc } => {
  if (isConcrete(x) && isConcrete(y)) {
    return { start: x.start, end: y.end, nodeType: x.nodeType };
  } else {
    return { nodeType: x.nodeType };
  }
};

// represents location of an empty AST node at the same location as `startAt`
const emptyLoc = (
  startAt: ASTNode<A>,
): { nodeType: NodeType; start?: SourceLoc; end?: SourceLoc } => {
  if (isConcrete(startAt)) {
    const { start } = startAt;
    return {
      start,
      end: start,
      nodeType: startAt.nodeType,
    };
  } else {
    return { nodeType: startAt.nodeType };
  }
};

const findInSubst = (
  subst: StySubst,
  id: string,
): SubstanceObject | undefined => {
  if (subst.tag === "CollectionSubst" && id in subst.groupby) {
    return subst.groupby[id];
  } else if (subst.tag === "StySubSubst" && id in subst.contents) {
    return subst.contents[id];
  } else {
    return undefined;
  }
};

export const resolveLhsStylePath = (
  { block, subst }: BlockInfo,
  assignment: BlockAssignment,
  original: Path<C>,
): Result<ResolvedStylePath<A>, StyleError> => {
  if (original.indices.length > 0) {
    return err({ tag: "AssignAccessError", path: original });
  }

  let curr: ResolvedStylePath<A> = { ...emptyLoc(original), tag: "Empty" };
  const firstPart: Identifier<A> =
    original.name.tag === "StyVar"
      ? original.name.contents
      : {
          ...original.name.contents,
          tag: "Identifier",
          value: `\`${original.name.contents.value}\``,
        };
  let parts: Identifier<A>[] = [firstPart, ...original.members];
  while (parts.length > 0) {
    const r = resolveLhsStylePathHelper(
      { block, subst },
      assignment,
      curr,
      parts,
    );
    if (r.isErr()) {
      return err(r.error);
    } else {
      const { result, remaining } = r.value;
      curr = result;
      parts = remaining;
    }
  }
  return ok(curr);
};

const resolveLhsStylePathHelper = (
  { block, subst }: BlockInfo,
  assignment: BlockAssignment,
  curr: ResolvedStylePath<A>,
  parts: Identifier<A>[],
): Result<
  {
    result: ResolvedStylePath<A>;
    remaining: Identifier<A>[];
  },
  StyleError
> => {
  if (parts.length === 0) {
    return ok({ result: curr, remaining: [] });
  }
  const [next, ...rest] = parts;
  const nextName = next.value;
  if (curr.tag === "Empty") {
    const subObj = findInSubst(subst, next.value);
    if (subObj !== undefined) {
      return ok({
        result: {
          ...loc(next),
          tag: "Substance",
          substanceName: subObjectToUniqueName(subObj),
          styleName: nextName,
        },
        remaining: rest,
      });
    } else if (assignment.globals.has(nextName)) {
      return ok({
        result: {
          ...loc(next),
          tag: "Namespace",
          name: nextName,
        },
        remaining: rest,
      });
    } else {
      if (block.tag === "LocalVarId") {
        const [blockId, substId] = block.contents;
        return ok({
          result: {
            ...loc(curr),
            tag: "Unnamed",
            blockId,
            substId,
          },
          // do not advance the pointer since we haven't actually resolved `next`
          // we only know that `next` is in the scope of `unnamed`
          remaining: parts,
        });
      } else {
        return ok({
          result: {
            ...loc(curr),
            tag: "Namespace",
            name: nextName,
          },
          // similar as above
          remaining: parts,
        });
      }
    }
  } else if (
    curr.tag === "Namespace" ||
    curr.tag === "Substance" ||
    curr.tag === "Unnamed"
  ) {
    return ok({
      result: {
        ...locs(curr, next),
        tag: "Object",
        parent: curr,
        name: nextName,
        shapeOrValue: "Unknown",
      },
      remaining: rest,
    });
  } else {
    // we now know that `curr` points to a shape object
    // hence `curr.parent` must be a scope.
    // otherwise we would have nested shapes, which is not allowed
    if (curr.parent.tag === "Object") {
      return err();
    } else {
      return ok({
        result: {
          ...locs(curr, next),
          tag: "Object",
          parent: {
            ...curr,
            tag: "Object",
            shapeOrValue: "Shape",
            parent: curr.parent,
            name: curr.name,
          },
          name: nextName,
          shapeOrValue: "Value",
        },
        remaining: rest,
      });
    }
  }
};

export const stylePathToNamespaceScope = (
  name: string,
): StylePathToNamespaceScope<A> => ({
  tag: "Namespace",
  name,
  nodeType: "SyntheticStyle",
});

export const stylePathToSubstanceScope = (
  name: string,
): StylePathToSubstanceScope<A> => ({
  tag: "Substance",
  substanceName: name,
  styleName: name,
  nodeType: "SyntheticStyle",
});

export const stylePathToUnnamedScope = (
  blockId: number,
  substId: number,
): StylePathToScope<A> => ({
  tag: "Unnamed",
  blockId,
  substId,
  nodeType: "SyntheticStyle",
});

export const makeStylePathToUnknownObject = (
  parent: StylePathToScope<A>,
  name: string,
): StylePathToObject<A> => ({
  tag: "Object",
  parent,
  name,
  shapeOrValue: "Unknown",
  nodeType: "SyntheticStyle",
});
