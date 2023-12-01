import { Shape } from "@penrose/core/dist/shapes/Shapes";
import * as ad from "@penrose/core/dist/types/ad";
import { prettyStmt } from "@penrose/core/dist/compiler/Substance";
import {
  ClipData,
  Color,
  PathDataV,
  Value,
} from "@penrose/core/dist/types/value";
import { zip2 } from "@penrose/core";
import { Locker } from "./locker.js";

export const lockShapes = (
  fromShapes: Shape<ad.Num>[],
  toShapes: Shape<ad.Num>[],
  causedBy: string[],
  locker: Locker,
) => {
  // Only care about values in these shapes
  const intersectShapes = fromShapes.filter((shape) =>
    shape.meta.causedBy.some((stmt) => causedBy.includes(prettyStmt(stmt))),
  );

  let pairsToLock: [Shape<ad.Num>, Shape<ad.Num>][] = [];
  for (const fromShape of intersectShapes) {
    const result = toShapes.find(
      (toShape) => toShape.name.contents === fromShape.name.contents,
    );
    if (result !== undefined) {
      pairsToLock.push([fromShape, result]);
    }
  }

  for (const [fromShape, toShape] of pairsToLock) {
    lockShape(fromShape, toShape, locker);
  }
};

const lockShape = (
  fromShape: Shape<ad.Num>,
  toShape: Shape<ad.Num>,
  locker: Locker,
) => {
  const fromName = fromShape.name.contents;
  if (fromShape.shapeType !== toShape.shapeType) {
    throw new Error(
      `Shape ${fromShape.name.contents} does not have the same shape type across sub1 and sub2`,
    );
  }

  // handle passthrough

  for (const [prop, fromValue] of fromShape.passthrough) {
    const toValue = toShape.passthrough.get(prop);
    if (toValue === undefined) {
      throw new Error(
        `Cannot lock sub2 ${fromName}.${prop} to value of sub1 ${fromName}.${prop} because the former does not exist`,
      );
    }
    lockValue(fromValue, toValue, locker);
  }

  // handle all other properties

  for (const [prop, fromValue] of Object.entries(fromShape)) {
    if (prop === "shapeType" || prop === "passthrough" || prop === "meta") {
      continue;
    }

    const toValue = (toShape as any)[prop];
    lockValue(fromValue as Value<ad.Num>, toValue as Value<ad.Num>, locker);
  }
};

const lockValue = (from: Value<ad.Num>, to: Value<ad.Num>, locker: Locker) => {
  if (from.tag === "FloatV" && to.tag === "FloatV") {
    lockNum(from.contents, to.contents, locker);
  } else if (from.tag === "BoolV" && to.tag === "BoolV") {
    if (from.contents !== to.contents) {
      throw new Error(
        `Cannot lock bool ${to.contents} to value of bool ${from.contents}`,
      );
    }
    // nothing to lock
    return;
  } else if (from.tag === "StrV" && to.tag === "StrV") {
    if (from.contents !== to.contents) {
      throw new Error(
        `Cannot lock string \"${to.contents}\" to value of string \"${from.contents}\"`,
      );
    }
    // nothing to lock
    return;
  } else if (from.tag === "PathDataV" && to.tag === "PathDataV") {
    lockPathDataV(from, to, locker);
  } else if (
    (from.tag === "PtListV" && to.tag === "PtListV") ||
    (from.tag === "MatrixV" && to.tag === "MatrixV") ||
    (from.tag === "LListV" && to.tag === "LListV")
  ) {
    if (from.contents.length !== to.contents.length) {
      throw new Error(
        `Cannot lock matrix of length ${to.contents.length} to value of matrix of length ${from.contents.length}`,
      );
    }
    zip2(from.contents, to.contents).map(([fromRow, toRow]) =>
      lockList(fromRow, toRow, locker),
    );
  } else if (from.tag === "ColorV" && to.tag === "ColorV") {
    lockColor(from.contents, to.contents, locker);
  } else if (
    (from.tag === "VectorV" && to.tag === "VectorV") ||
    (from.tag === "ListV" && to.tag === "ListV") ||
    (from.tag === "TupV" && to.tag === "TupV")
  ) {
    lockList(from.contents, to.contents, locker);
  } else if (from.tag === "ShapeListV" && to.tag === "ShapeListV") {
    if (from.contents.length !== to.contents.length) {
      throw new Error(
        `Cannot lock shape list of length ${to.contents.length} to value of shape list of length ${from.contents.length}`,
      );
    }
    zip2(from.contents, to.contents).map(([from, to]) =>
      lockShape(from, to, locker),
    );
  } else if (from.tag === "ClipDataV" && to.tag === "ClipDataV") {
    lockClipData(from.contents, to.contents, locker);
  } else {
    throw new Error(`Cannot lock ${to.tag} to value of ${from.tag}`);
  }
};

const lockPathDataV = (
  from: PathDataV<ad.Num>,
  to: PathDataV<ad.Num>,
  locker: Locker,
) => {
  if (from.contents.length !== to.contents.length) {
    throw new Error("yyy");
  }
  zip2(from.contents, to.contents).map(([cmd1, cmd2]) => {
    if (cmd1.cmd !== cmd2.cmd) {
      throw new Error("yyy");
    }
    zip2(cmd1.contents, cmd2.contents).map(([sp1, sp2]) => {
      if (sp1.tag !== sp2.tag) {
        throw new Error("yyy");
      }
      zip2(sp1.contents, sp2.contents).map(([v1, v2]) =>
        lockNum(v1, v2, locker),
      );
    });
  });
};

const lockColor = (from: Color<ad.Num>, to: Color<ad.Num>, locker: Locker) => {
  if (from.tag === "NONE" && to.tag === "NONE") {
    // nothing to lock
    return;
  } else if (
    (from.tag === "RGBA" && to.tag === "RGBA") ||
    (from.tag === "HSVA" && to.tag === "HSVA")
  ) {
    lockList(from.contents, to.contents, locker);
  } else {
    throw new Error(
      `Cannot lock color of ${to.tag} to value of color of ${from.tag}`,
    );
  }
};

const lockClipData = (
  from: ClipData<ad.Num>,
  to: ClipData<ad.Num>,
  locker: Locker,
) => {
  if (from.tag === "NoClip" && to.tag === "NoClip") {
    // nothing to lock
    return;
  } else if (from.tag === "Clip" && to.tag === "Clip") {
    lockShape(from.contents, to.contents, locker);
  } else {
    throw new Error(
      `Cannot lock clip data (${to.tag}) to value of clip data (${from.tag})`,
    );
  }
};

const lockList = (from: ad.Num[], to: ad.Num[], locker: Locker) => {
  if (from.length !== to.length) {
    throw new Error(
      `Cannot lock list of length ${to.length} to value of list of length ${from.length}`,
    );
  }
  zip2(from, to).map(([fromNum, toNum]) => lockNum(fromNum, toNum, locker));
};

const lockNum = (from: ad.Num, to: ad.Num, locker: Locker) => {
  if (typeof from === "number" && typeof to === "number") {
    if (from !== to) {
      throw new Error(
        `Cannot lock constant number ${to} to value of constant number ${from}`,
      );
    }
    // nothing to lock
    return;
  } else if (
    (typeof from === "number" && typeof to !== "number") ||
    (typeof from !== "number" && typeof to === "number")
  ) {
    throw new Error("xxx");
  } else if (typeof from !== "number" && typeof to !== "number") {
    return lockComputedNum(from, to, locker);
  }
  throw new Error("xxx");
};

const lockComputedNum = (
  from: Exclude<ad.Num, number>,
  to: Exclude<ad.Num, number>,
  locker: Locker,
) => {
  if (from.tag === "Var" && to.tag === "Var") {
    locker(from, to);
    return;
  } else if (from.tag === "Unary" && to.tag === "Unary") {
    if (from.unop !== to.unop) {
      throw new Error(
        `Cannot lock Unary(${from.unop}) to value of Unary(${to.unop})`,
      );
    }
    lockNum(from.param, to.param, locker);
  } else if (from.tag === "Binary" && to.tag === "Binary") {
    if (from.binop !== to.binop) {
      throw new Error(
        `Cannot lock Binary(${from.binop}) to value of Binary(${to.binop})`,
      );
    }
    lockNum(from.left, to.left, locker);
    lockNum(from.right, to.right, locker);
  } else if (from.tag === "Ternary" && to.tag === "Ternary") {
    lockBool(from.cond, to.cond, locker);
    lockNum(from.then, to.then, locker);
    lockNum(from.els, to.els, locker);
  } else if (from.tag === "Nary" && to.tag === "Nary") {
    if (from.op !== to.op) {
      throw new Error(
        `Cannot lock Nary(${from.op}) to value of Nary(${to.op})`,
      );
    }
    zip2(from.params, to.params).map(([fromParam, toParam]) =>
      lockNum(fromParam, toParam, locker),
    );
  } else if (from.tag === "Index" && to.tag === "Index") {
    if (from.index !== to.index) {
      throw new Error(
        `Cannot lock Index(index=${from.index}) to value of Index(index=${to.index})`,
      );
    }
    if (from.vec.degree !== to.vec.degree) {
      throw new Error(
        `Cannot lock Index(vec.degree=${from.vec.degree}) to value of Index(vec.degree=${to.vec.degree})`,
      );
    }
    zip2(from.vec.coeffs, to.vec.coeffs).map(([fromParam, toParam]) =>
      lockNum(fromParam, toParam, locker),
    );
  }

  throw new Error(`Cannot lock ${to.tag} to value of ${from.tag}`);
};

const lockBool = (from: ad.Bool, to: ad.Bool, locker: Locker) => {
  if (from.tag === "Comp" && to.tag === "Comp") {
    if (from.binop !== to.binop) {
    }
    lockNum(from.left, to.left, locker);
    lockNum(from.right, to.right, locker);
  } else if (from.tag === "Logic" && to.tag === "Logic") {
    if (from.binop !== to.binop) {
      throw new Error(
        `Cannot lock Logic(${from.binop}) to value of Logic(${to.binop})`,
      );
    }
    lockBool(from.left, to.left, locker);
    lockBool(from.right, to.right, locker);
  } else if (from.tag === "Not" && to.tag === "Not") {
    lockBool(from.param, to.param, locker);
  }

  throw new Error(`Cannot lock ${to.tag} to value of ${from.tag}`);
};
