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
import { Freezer } from "./freezer.js";

export const freezeShapes = (
  fromShapes: Shape<ad.Num>[],
  toShapes: Shape<ad.Num>[],
  causedBy: string[],
  freezer: Freezer,
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
    freezeShape(fromShape, toShape, freezer);
  }
};

const freezeShape = (
  fromShape: Shape<ad.Num>,
  toShape: Shape<ad.Num>,
  freezer: Freezer,
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
    freezeValue(fromValue, toValue, freezer);
  }

  // handle all other properties

  for (const [prop, fromValue] of Object.entries(fromShape)) {
    if (prop === "shapeType" || prop === "passthrough" || prop === "meta") {
      continue;
    }

    const toValue = (toShape as any)[prop];
    freezeValue(fromValue as Value<ad.Num>, toValue as Value<ad.Num>, freezer);
  }
};

const freezeValue = (
  from: Value<ad.Num>,
  to: Value<ad.Num>,
  freezer: Freezer,
) => {
  if (from.tag === "FloatV" && to.tag === "FloatV") {
    freezeNum(from.contents, to.contents, freezer);
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
    freezePathDataV(from, to, freezer);
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
      freezeList(fromRow, toRow, freezer),
    );
  } else if (from.tag === "ColorV" && to.tag === "ColorV") {
    freezeColor(from.contents, to.contents, freezer);
  } else if (
    (from.tag === "VectorV" && to.tag === "VectorV") ||
    (from.tag === "ListV" && to.tag === "ListV") ||
    (from.tag === "TupV" && to.tag === "TupV")
  ) {
    freezeList(from.contents, to.contents, freezer);
  } else if (from.tag === "ShapeListV" && to.tag === "ShapeListV") {
    if (from.contents.length !== to.contents.length) {
      throw new Error(
        `Cannot lock shape list of length ${to.contents.length} to value of shape list of length ${from.contents.length}`,
      );
    }
    zip2(from.contents, to.contents).map(([from, to]) =>
      freezeShape(from, to, freezer),
    );
  } else if (from.tag === "ClipDataV" && to.tag === "ClipDataV") {
    freezeClipData(from.contents, to.contents, freezer);
  } else {
    throw new Error(`Cannot lock ${to.tag} to value of ${from.tag}`);
  }
};

const freezePathDataV = (
  from: PathDataV<ad.Num>,
  to: PathDataV<ad.Num>,
  freezer: Freezer,
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
        freezeNum(v1, v2, freezer),
      );
    });
  });
};

const freezeColor = (
  from: Color<ad.Num>,
  to: Color<ad.Num>,
  freezer: Freezer,
) => {
  if (from.tag === "NONE" && to.tag === "NONE") {
    // nothing to lock
    return;
  } else if (
    (from.tag === "RGBA" && to.tag === "RGBA") ||
    (from.tag === "HSVA" && to.tag === "HSVA")
  ) {
    freezeList(from.contents, to.contents, freezer);
  } else {
    throw new Error(
      `Cannot lock color of ${to.tag} to value of color of ${from.tag}`,
    );
  }
};

const freezeClipData = (
  from: ClipData<ad.Num>,
  to: ClipData<ad.Num>,
  freezer: Freezer,
) => {
  if (from.tag === "NoClip" && to.tag === "NoClip") {
    // nothing to lock
    return;
  } else if (from.tag === "Clip" && to.tag === "Clip") {
    freezeShape(from.contents, to.contents, freezer);
  } else {
    throw new Error(
      `Cannot lock clip data (${to.tag}) to value of clip data (${from.tag})`,
    );
  }
};

const freezeList = (from: ad.Num[], to: ad.Num[], freezer: Freezer) => {
  if (from.length !== to.length) {
    throw new Error(
      `Cannot lock list of length ${to.length} to value of list of length ${from.length}`,
    );
  }
  zip2(from, to).map(([fromNum, toNum]) => freezeNum(fromNum, toNum, freezer));
};

const freezeNum = (from: ad.Num, to: ad.Num, freezer: Freezer) => {
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
    return freezeComputedNum(from, to, freezer);
  }
  throw new Error("xxx");
};

const freezeComputedNum = (
  from: Exclude<ad.Num, number>,
  to: Exclude<ad.Num, number>,
  freezer: Freezer,
) => {
  if (from.tag === "Var" && to.tag === "Var") {
    freezer(from, to);
    return;
  } else if (from.tag === "Unary" && to.tag === "Unary") {
    if (from.unop !== to.unop) {
      throw new Error(
        `Cannot lock Unary(${from.unop}) to value of Unary(${to.unop})`,
      );
    }
    freezeNum(from.param, to.param, freezer);
  } else if (from.tag === "Binary" && to.tag === "Binary") {
    if (from.binop !== to.binop) {
      throw new Error(
        `Cannot lock Binary(${from.binop}) to value of Binary(${to.binop})`,
      );
    }
    freezeNum(from.left, to.left, freezer);
    freezeNum(from.right, to.right, freezer);
  } else if (from.tag === "Ternary" && to.tag === "Ternary") {
    freezeBool(from.cond, to.cond, freezer);
    freezeNum(from.then, to.then, freezer);
    freezeNum(from.els, to.els, freezer);
  } else if (from.tag === "Nary" && to.tag === "Nary") {
    if (from.op !== to.op) {
      throw new Error(
        `Cannot lock Nary(${from.op}) to value of Nary(${to.op})`,
      );
    }
    zip2(from.params, to.params).map(([fromParam, toParam]) =>
      freezeNum(fromParam, toParam, freezer),
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
      freezeNum(fromParam, toParam, freezer),
    );
  }

  throw new Error(`Cannot lock ${to.tag} to value of ${from.tag}`);
};

const freezeBool = (from: ad.Bool, to: ad.Bool, freezer: Freezer) => {
  if (from.tag === "Comp" && to.tag === "Comp") {
    if (from.binop !== to.binop) {
    }
    freezeNum(from.left, to.left, freezer);
    freezeNum(from.right, to.right, freezer);
  } else if (from.tag === "Logic" && to.tag === "Logic") {
    if (from.binop !== to.binop) {
      throw new Error(
        `Cannot lock Logic(${from.binop}) to value of Logic(${to.binop})`,
      );
    }
    freezeBool(from.left, to.left, freezer);
    freezeBool(from.right, to.right, freezer);
  } else if (from.tag === "Not" && to.tag === "Not") {
    freezeBool(from.param, to.param, freezer);
  }

  throw new Error(`Cannot lock ${to.tag} to value of ${from.tag}`);
};
