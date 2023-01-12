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
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed not locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open not locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});

describe("isConvex", () => {
  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed non-convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open non-convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});

describe("isEquidistant", () => {
  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed quidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquidistant(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed quidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquidistant(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open non-equidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquidistant(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open non-equidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquidistant(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});

describe("isEquiangular", () => {
  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _closed_paths[666]],
  ])("closed non-equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[666]],
    ["Polygon", _polygons[666]],
    ["Path", _open_paths[666]],
  ])("open non-equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeGreaterThan(0.1);
  });
});
