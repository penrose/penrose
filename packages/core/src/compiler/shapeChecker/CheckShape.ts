import { Result } from "true-myth";
import { dummyIdentifier } from "../../engine/EngineUtils";
import { Circle } from "../../shapes/Circle";
import { Ellipse } from "../../shapes/Ellipse";
import { Equation } from "../../shapes/Equation";
import { Group } from "../../shapes/Group";
import { Image } from "../../shapes/Image";
import { Line } from "../../shapes/Line";
import { Path } from "../../shapes/Path";
import { Polygon } from "../../shapes/Polygon";
import { Polyline } from "../../shapes/Polyline";
import { Rectangle } from "../../shapes/Rectangle";
import { Shape, ShapeType } from "../../shapes/Shapes";
import { Text } from "../../shapes/Text";
import * as ad from "../../types/ad";
import { StyleError } from "../../types/errors";
import { Translation } from "../../types/styleSemantics";
import {
  checkArrow,
  checkCenter,
  checkCorner,
  checkFill,
  checkNamed,
  checkPoly,
  checkProp,
  checkRect,
  checkRotate,
  checkScale,
  checkString,
  checkStroke,
} from "./CheckShapeHierarchyProps";
import {
  checkFloatV,
  checkPathDataV,
  checkShapeListV,
  checkStrV,
  checkVectorV,
} from "./CheckValues";

const { err, ok } = Result;

export const checkShape = (
  shapeType: ShapeType,
  path: string,
  trans: Translation
): Result<Shape<ad.Num>, StyleError> => {
  switch (shapeType) {
    case "Circle":
      return checkCircle(path, trans);
    case "Ellipse":
      return checkEllipse(path, trans);
    case "Equation":
      return checkEquation(path, trans);
    case "Image":
      return checkImage(path, trans);
    case "Line":
      return checkLine(path, trans);
    case "Path":
      return checkPath(path, trans);
    case "Polygon":
      return checkPolygon(path, trans);
    case "Polyline":
      return checkPolyline(path, trans);
    case "Rectangle":
      return checkRectangle(path, trans);
    case "Text":
      return checkText(path, trans);
    case "Group":
      return checkGroup(path, trans);
    default:
      return err({
        tag: "InvalidGPITypeError",
        givenType: dummyIdentifier(shapeType, "Style"),
      });
  }
};

export const checkCircle = (
  path: string,
  trans: Translation
): Result<Circle<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const center = checkCenter(path, trans);
  if (center.isErr()) return err(center.error);

  const r = checkProp(path, "r", trans, checkFloatV);
  if (r.isErr()) return err(r.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...center.value,
    r: r.value,
    passthrough: new Map(),
    shapeType: "Circle",
  });
};

export const checkEllipse = (
  path: string,
  trans: Translation
): Result<Ellipse<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const center = checkCenter(path, trans);
  if (center.isErr()) return err(center.error);

  const rx = checkProp(path, "rx", trans, checkFloatV);
  if (rx.isErr()) return err(rx.error);

  const ry = checkProp(path, "ry", trans, checkFloatV);
  if (ry.isErr()) return err(ry.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...center.value,
    rx: rx.value,
    ry: ry.value,
    passthrough: new Map(),
    shapeType: "Ellipse",
  });
};

export const checkEquation = (
  path: string,
  trans: Translation
): Result<Equation<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const center = checkCenter(path, trans);
  if (center.isErr()) return err(center.error);

  const rect = checkRect(path, trans);
  if (rect.isErr()) return err(rect.error);

  const rotate = checkRotate(path, trans);
  if (rotate.isErr()) return err(rotate.error);

  const string = checkString(path, trans);
  if (string.isErr()) return err(string.error);

  return ok({
    ...named.value,
    ...fill.value,
    ...center.value,
    ...rect.value,
    ...rotate.value,
    ...string.value,
    passthrough: new Map(),
    shapeType: "Equation",
  });
};

export const checkGroup = (
  path: string,
  trans: Translation
): Result<Group<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const shapes = checkProp(path, "shapes", trans, checkShapeListV);
  if (shapes.isErr()) return err(shapes.error);

  return ok({
    ...named.value,
    shapes: shapes.value,
    passthrough: new Map(),
    shapeType: "Group",
  });
};

export const checkImage = (
  path: string,
  trans: Translation
): Result<Image<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const center = checkCenter(path, trans);
  if (center.isErr()) return err(center.error);

  const rect = checkRect(path, trans);
  if (rect.isErr()) return err(rect.error);

  const rotate = checkRotate(path, trans);
  if (rotate.isErr()) return err(rotate.error);

  const href = checkProp(path, "href", trans, checkStrV);
  if (href.isErr()) return err(href.error);

  return ok({
    ...named.value,
    ...center.value,
    ...rect.value,
    ...rotate.value,
    href: href.value,
    passthrough: new Map(),
    shapeType: "Image",
  });
};

export const checkLine = (
  path: string,
  trans: Translation
): Result<Line<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const arrow = checkArrow(path, trans);
  if (arrow.isErr()) return err(arrow.error);

  const start = checkProp(path, "start", trans, checkVectorV);
  if (start.isErr()) return err(start.error);

  const end = checkProp(path, "end", trans, checkVectorV);
  if (end.isErr()) return err(end.error);

  const strokeLinecap = checkProp(path, "strokeLinecap", trans, checkStrV);
  if (strokeLinecap.isErr()) return err(strokeLinecap.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...arrow.value,
    start: start.value,
    end: end.value,
    strokeLinecap: strokeLinecap.value,
    passthrough: new Map(),
    shapeType: "Line",
  });
};

export const checkPath = (
  path: string,
  trans: Translation
): Result<Path<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const arrow = checkArrow(path, trans);
  if (arrow.isErr()) return err(arrow.error);

  const d = checkProp(path, "d", trans, checkPathDataV);
  if (d.isErr()) return err(d.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...arrow.value,
    d: d.value,
    passthrough: new Map(),
    shapeType: "Path",
  });
};

export const checkPolygon = (
  path: string,
  trans: Translation
): Result<Polygon<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const scale = checkScale(path, trans);
  if (scale.isErr()) return err(scale.error);

  const poly = checkPoly(path, trans);
  if (poly.isErr()) return err(poly.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...scale.value,
    ...poly.value,
    passthrough: new Map(),
    shapeType: "Polygon",
  });
};

export const checkPolyline = (
  path: string,
  trans: Translation
): Result<Polyline<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const scale = checkScale(path, trans);
  if (scale.isErr()) return err(scale.error);

  const poly = checkPoly(path, trans);
  if (poly.isErr()) return err(poly.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...scale.value,
    ...poly.value,
    passthrough: new Map(),
    shapeType: "Polyline",
  });
};

export const checkRectangle = (
  path: string,
  trans: Translation
): Result<Rectangle<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const center = checkCenter(path, trans);
  if (center.isErr()) return err(center.error);

  const rotate = checkRotate(path, trans);
  if (rotate.isErr()) return err(rotate.error);

  const rect = checkRect(path, trans);
  if (rect.isErr()) return err(rect.error);

  const corner = checkCorner(path, trans);
  if (corner.isErr()) return err(corner.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...center.value,
    ...rotate.value,
    ...rect.value,
    ...corner.value,
    passthrough: new Map(),
    shapeType: "Rectangle",
  });
};

export const checkText = (
  path: string,
  trans: Translation
): Result<Text<ad.Num>, StyleError> => {
  const named = checkNamed(path, trans);
  if (named.isErr()) return err(named.error);

  const stroke = checkStroke(path, trans);
  if (stroke.isErr()) return err(stroke.error);

  const fill = checkFill(path, trans);
  if (fill.isErr()) return err(fill.error);

  const center = checkCenter(path, trans);
  if (center.isErr()) return err(center.error);

  const rect = checkRect(path, trans);
  if (rect.isErr()) return err(rect.error);

  const rotate = checkRotate(path, trans);
  if (rotate.isErr()) return err(rotate.error);

  const string = checkString(path, trans);
  if (string.isErr()) return err(string.error);

  const visibility = checkProp(path, "visibility", trans, checkStrV);
  if (visibility.isErr()) return err(visibility.error);

  const fontFamily = checkProp(path, "fontFamily", trans, checkStrV);
  if (fontFamily.isErr()) return err(fontFamily.error);

  const fontSizeAdjust = checkProp(path, "fontSizeAdjust", trans, checkStrV);
  if (fontSizeAdjust.isErr()) return err(fontSizeAdjust.error);

  const fontStretch = checkProp(path, "fontStretch", trans, checkStrV);
  if (fontStretch.isErr()) return err(fontStretch.error);

  const fontStyle = checkProp(path, "fontStyle", trans, checkStrV);
  if (fontStyle.isErr()) return err(fontStyle.error);

  const fontVariant = checkProp(path, "fontVariant", trans, checkStrV);
  if (fontVariant.isErr()) return err(fontVariant.error);

  const fontWeight = checkProp(path, "fontWeight", trans, checkStrV);
  if (fontWeight.isErr()) return err(fontWeight.error);

  const textAnchor = checkProp(path, "textAnchor", trans, checkStrV);
  if (textAnchor.isErr()) return err(textAnchor.error);

  const lineHeight = checkProp(path, "lineHeight", trans, checkStrV);
  if (lineHeight.isErr()) return err(lineHeight.error);

  const alignmentBaseline = checkProp(
    path,
    "alignmentBaseline",
    trans,
    checkStrV
  );
  if (alignmentBaseline.isErr()) return err(alignmentBaseline.error);

  const dominantBaseline = checkProp(
    path,
    "dominantBaseline",
    trans,
    checkStrV
  );
  if (dominantBaseline.isErr()) return err(dominantBaseline.error);

  const ascent = checkProp(path, "ascent", trans, checkFloatV);
  if (ascent.isErr()) return err(ascent.error);

  const descent = checkProp(path, "descent", trans, checkFloatV);
  if (descent.isErr()) return err(descent.error);

  return ok({
    ...named.value,
    ...stroke.value,
    ...fill.value,
    ...center.value,
    ...rotate.value,
    ...rect.value,
    ...string.value,
    visibility: visibility.value,
    fontFamily: fontFamily.value,
    fontSizeAdjust: fontSizeAdjust.value,
    fontStretch: fontStretch.value,
    fontStyle: fontStyle.value,
    fontVariant: fontVariant.value,
    fontWeight: fontWeight.value,
    textAnchor: textAnchor.value,
    lineHeight: lineHeight.value,
    alignmentBaseline: alignmentBaseline.value,
    dominantBaseline: dominantBaseline.value,
    ascent: ascent.value,
    descent: descent.value,
    passthrough: new Map(),
    shapeType: "Text",
  });
};
