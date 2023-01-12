import { constrDictCurves, equivalued } from "contrib/CurveConstraints";
import { numOf } from "contrib/Utils";
import {
  _closed_paths,
  _open_paths,
  _polygons,
  _polylines,
} from "contrib/__testfixtures__/TestShapes.input";

describe("equivalued", () => {
  it.each([[[2]], [[3, 3, 3, 3, 3]], [[-5, -5, -5]], [[-5, -5, -5]]])(
    "same numbers",
    (values: number[]) => {
      const result = equivalued(values);
      expect(numOf(result)).toBeCloseTo(0, 4);
    }
  );

  it.each([
    [[3, 1], 2],
    [[-1, 0, 1], 2],
    [[-5, -1], 8],
  ])("different numbers", (values: number[], expected: number) => {
    const result = equivalued(values);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("isLocallyConvex", () => {
  it.each([
    ["Polyline", _polylines[0]],
    ["Polygon", _polygons[0]],
    ["Path", _closed_paths[0]],
    ["Path", _open_paths[0]],
    ["Polyline", _polylines[2]],
    ["Polygon", _polygons[2]],
    ["Path", _closed_paths[2]],
    ["Path", _open_paths[2]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
    ["Path", _closed_paths[8]],
    ["Path", _open_paths[8]],
  ])("locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[5]],
    ["Polygon", _polygons[5]],
    ["Path", _closed_paths[5]],
    ["Path", _open_paths[5]],
    ["Polyline", _polylines[10]],
    ["Polygon", _polygons[10]],
    ["Path", _closed_paths[10]],
    ["Path", _open_paths[10]],
  ])("not locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});

describe("isConvex", () => {
  it.each([
    ["Polygon", _polygons[0]],
    ["Path", _closed_paths[0]],
    ["Polygon", _polygons[2]],
    ["Path", _closed_paths[2]],
    ["Polygon", _polygons[6]],
    ["Path", _closed_paths[6]],
    ["Polygon", _polygons[7]],
    ["Path", _closed_paths[7]],
  ])("convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polygon", _polygons[5]],
    ["Path", _closed_paths[5]],
    ["Polygon", _polygons[8]],
    ["Path", _closed_paths[8]],
  ])("non-convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});

describe("isEquidistant", () => {
  it.each([
    ["Polyline", _polylines[6]],
    ["Polygon", _polygons[6]],
    ["Path", _closed_paths[6]],
    ["Path", _open_paths[6]],
    ["Polyline", _polylines[9]],
    ["Polygon", _polygons[9]],
    ["Path", _closed_paths[9]],
    ["Path", _open_paths[9]],
    ["Polyline", _polylines[7]],
    ["Polygon", _polygons[7]],
    ["Path", _closed_paths[7]],
    ["Path", _open_paths[7]],
  ])("equidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquidistant(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[7]],
    ["Polygon", _polygons[7]],
    ["Path", _closed_paths[7]],
    ["Path", _open_paths[7]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
    ["Path", _closed_paths[8]],
    ["Path", _open_paths[8]],
    ["Polyline", _polylines[9]],
    ["Polygon", _polygons[9]],
    ["Path", _closed_paths[9]],
    ["Path", _open_paths[9]],
  ])("non-equidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquidistant(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});

describe("isEquiangular", () => {
  it.each([
    ["Polyline", _polylines[6]],
    ["Polygon", _polygons[6]],
    ["Path", _closed_paths[6]],
    ["Path", _open_paths[6]],
    ["Polyline", _polylines[7]],
    ["Polygon", _polygons[7]],
    ["Path", _closed_paths[7]],
    ["Path", _open_paths[7]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
    ["Path", _closed_paths[8]],
    ["Path", _open_paths[8]],
  ])("equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[0]],
    ["Polygon", _polygons[0]],
    ["Path", _closed_paths[0]],
    ["Path", _open_paths[0]],
    ["Polyline", _polylines[5]],
    ["Polygon", _polygons[5]],
    ["Path", _closed_paths[5]],
    ["Path", _open_paths[5]],
    ["Polyline", _polylines[9]],
    ["Polygon", _polygons[9]],
    ["Path", _closed_paths[9]],
    ["Path", _open_paths[9]],
  ])("non-equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});
