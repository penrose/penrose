import { isConcrete } from "../engine/EngineUtils.js";
import { A, ASTNode, Identifier, NodeType, SourceLoc } from "../types/ast.js";
import { StyleError } from "../types/errors.js";
import { Expr, FunctionCall, InlineComparison, Path } from "../types/style.js";
import {
  ResolvedExpr,
  ResolvedFunctionCall,
  ResolvedInlineComparison,
  ResolvedPath,
  ResolvedPropertyDecl,
  ResolvedStylePath,
  ResolvedUnindexedStylePath,
  StylePathToNamespaceScope,
  StylePathToScope,
  StylePathToSubstanceScope,
  StylePathToUnindexedObject,
  UnindexedStylePath,
} from "../types/stylePathResolution.js";
import {
  Assignment,
  BlockInfo,
  StySubst,
  SubstanceObject,
} from "../types/styleSemantics.js";
import { Result, all, err, ok } from "../utils/Error.js";
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
): SubstanceObject | SubstanceObject[] | undefined => {
  if (subst.tag === "CollectionSubst") {
    if (id === subst.collName) {
      return subst.collContent;
    }
    if (id in subst.groupby) {
      return subst.groupby[id];
    }
  }

  if (subst.tag === "StySubSubst" && id in subst.contents) {
    return subst.contents[id];
  }

  return undefined;
};

export const resolveLhsStylePath = (
  block: BlockInfo,
  assignment: Assignment,
  original: Path<A>,
): Result<ResolvedUnindexedStylePath<A>, StyleError> => {
  const resolved = resolveStylePath(block, assignment, original);
  if (resolved.isErr()) return err(resolved.error);

  if (resolved.value.tag === "Object") {
    const { access } = resolved.value;
    if (access.tag === "Index") {
      // AssignAccessError
      return err({
        tag: "AssignAccessError",
        path: resolved.value,
      });
    } else {
      return ok({
        ...resolved.value,
        access,
      });
    }
  } else {
    return ok(resolved.value);
  }
};

export const resolveStylePath = (
  block: BlockInfo,
  assignment: Assignment,
  original: Path<A>,
): Result<ResolvedStylePath<A>, StyleError> => {
  const withoutIndex = resolveStylePathWithoutIndex(
    block,
    assignment,
    original,
  );
  if (withoutIndex.isErr()) {
    return err(withoutIndex.error);
  }

  const { indices } = original;

  if (indices.length === 0) {
    return ok(withoutIndex.value);
  }

  if (withoutIndex.value.tag !== "Object") {
    // numerically indexing into an non-object
    return err({
      tag: "UnindexableItemError",
      expr: withoutIndex.value,
    });
  }

  const resolvedIndices = all(
    indices.map((index) => resolveStyleExpr(block, assignment, index)),
  );

  if (resolvedIndices.isErr()) {
    return err(resolvedIndices.error[0]);
  }

  const res: ResolvedStylePath<A> = {
    ...original,
    tag: "Object",
    access: {
      tag: "Index",
      parent: withoutIndex.value,
      indices: resolvedIndices.value,
    },
  };

  return ok(res);
};

export const resolveStylePathWithoutIndex = (
  { block, subst }: BlockInfo,
  assignment: Assignment,
  original: Omit<Path<A>, "indices">,
): Result<ResolvedUnindexedStylePath<A>, StyleError> => {
  let curr: UnindexedStylePath<A> = {
    ...emptyLoc(original),
    tag: "Empty",
  };
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
    const r = resolveStylePathHelper({ block, subst }, assignment, curr, parts);
    if (r.isErr()) {
      return err(r.error);
    } else {
      const { result, remaining } = r.value;
      curr = result;
      parts = remaining;
    }
  }

  if (curr.tag === "Empty" || curr.tag === "Unnamed") {
    // should never happen
    throw new Error(
      "resolved into an Empty or Unnamed path which should not happen",
    );
  } else return ok(curr);
};

const resolveStylePathHelper = (
  { block, subst }: BlockInfo,
  assignment: Assignment,
  curr: UnindexedStylePath<A>,
  parts: Identifier<A>[],
): Result<
  {
    result: UnindexedStylePath<A>;
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
      if (subObj instanceof Array) {
        return ok({
          result: {
            ...loc(next),
            tag: "Collection",
            substanceObjects: subObj,
            styleName: nextName,
          },
          remaining: rest,
        });
      } else {
        return ok({
          result: {
            ...loc(next),
            tag: "Substance",
            substanceObject: subObj,
            styleName: nextName,
          },
          remaining: rest,
        });
      }
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
        access: {
          tag: "Member",
          parent: curr,
          name: nextName,
        },
      },
      remaining: rest,
    });
  } else if (curr.tag === "Collection") {
    return err({
      tag: "CollectionMemberAccessError",
      path: curr,
      field: nextName,
    });
  } else {
    // if both parent and parent's parent are objects, this is not a well-formed path
    const newPath: StylePathToUnindexedObject<A> = {
      ...locs(curr, next),
      tag: "Object",
      access: {
        tag: "Member",
        parent: curr,
        name: nextName,
      },
    };
    if (
      curr.access.parent.tag === "Object" &&
      curr.access.parent.access.parent.tag === "Object"
    ) {
      return err({
        tag: "NonWellFormedPathError",
        path: newPath,
      });
    }
    return ok({
      result: newPath,
      remaining: rest,
    });
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
  obj: SubstanceObject,
): StylePathToSubstanceScope<A> => ({
  tag: "Substance",
  substanceObject: obj,
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
const resolvePath = (
  block: BlockInfo,
  assignment: Assignment,
  p: Path<A>,
): Result<ResolvedPath<A>, StyleError> => {
  const resolved = resolveStylePath(block, assignment, p);
  if (resolved.isErr()) {
    return err(resolved.error);
  }
  return ok({
    ...p,
    tag: "ResolvedPath",
    contents: resolved.value,
  });
};

const resolveStyleFunctionCall = (
  block: BlockInfo,
  assignment: Assignment,
  body: FunctionCall<A> | InlineComparison<A>,
): Result<
  ResolvedFunctionCall<A> | ResolvedInlineComparison<A>,
  StyleError
> => {
  const resolvee = (e: Expr<A>) => resolveStyleExpr(block, assignment, e);
  if (body.tag === "InlineComparison") {
    const a1 = resolvee(body.arg1);
    if (a1.isErr()) return err(a1.error);
    const a2 = resolvee(body.arg2);
    if (a2.isErr()) return err(a2.error);
    return ok({
      ...body,
      arg1: a1.value,
      arg2: a2.value,
    });
  } else {
    const a = all(body.args.map(resolvee));
    if (a.isErr()) return err(a.error[0]);
    return ok({
      ...body,
      args: a.value,
    });
  }
};

export const resolveStyleExpr = (
  block: BlockInfo,
  assignment: Assignment,
  expr: Expr<A>,
): Result<ResolvedExpr<A>, StyleError> => {
  const resolvee = (e: Expr<A>) => resolveStyleExpr(block, assignment, e);
  const resolvep = (p: Path<A>): Result<ResolvedPath<A>, StyleError> =>
    resolvePath(block, assignment, p);
  switch (expr.tag) {
    case "Fix":
    case "Vary":
    case "BoolLit":
    case "StringLit":
    case "ColorLit":
      return ok(expr);
    case "BinOp": {
      const l = resolvee(expr.left);
      if (l.isErr()) return err(l.error);
      const r = resolvee(expr.right);
      if (r.isErr()) return err(r.error);
      return ok({
        ...expr,
        left: l.value,
        right: r.value,
      });
    }
    case "UOp": {
      const a = resolvee(expr.arg);
      if (a.isErr()) return err(a.error);
      return ok({
        ...expr,
        arg: a.value,
      });
    }
    case "Layering": {
      const l = resolvep(expr.left);
      if (l.isErr()) return err(l.error);
      const r = all(expr.right.map(resolvep));
      if (r.isErr()) return err(r.error[0]);
      return ok({
        ...expr,
        left: l.value,
        right: r.value,
      });
    }
    case "List":
    case "Vector": {
      const c = all(expr.contents.map(resolvee));
      if (c.isErr()) return err(c.error[0]);
      return ok({
        ...expr,
        contents: c.value,
      });
    }
    case "Tuple": {
      const c0 = resolvee(expr.contents[0]);
      if (c0.isErr()) return err(c0.error);
      const c1 = resolvee(expr.contents[1]);
      if (c1.isErr()) return err(c1.error);
      return ok({
        ...expr,
        contents: [c0.value, c1.value],
      });
    }
    case "CompApp": {
      const a = all(expr.args.map(resolvee));
      if (a.isErr()) return err(a.error[0]);
      return ok({
        ...expr,
        args: a.value,
      });
    }
    case "ObjFn":
    case "ConstrFn": {
      const b = resolveStyleFunctionCall(block, assignment, expr.body);
      if (b.isErr()) return err(b.error);
      return ok({
        ...expr,
        body: b.value,
      });
    }
    case "GPIDecl": {
      const ps = all(
        expr.properties.map(
          (pDecl): Result<ResolvedPropertyDecl<A>, StyleError> => {
            const v = resolvee(pDecl.value);
            if (v.isErr()) return err(v.error);
            return ok({
              ...pDecl,
              value: v.value,
            });
          },
        ),
      );
      if (ps.isErr()) return err(ps.error[0]);
      return ok({
        ...expr,
        properties: ps.value,
      });
    }
    case "UnaryStyVarExpr": {
      const a = resolvep(expr.arg);
      if (a.isErr()) return err(a.error);
      return ok({
        ...expr,
        arg: a.value,
      });
    }
    case "CollectionAccess": {
      const n = resolvep(expr.name);
      if (n.isErr()) return err(n.error);
      return ok({
        ...expr,
        name: n.value,
      });
    }
    case "Path":
      return resolvep(expr);
  }
};
