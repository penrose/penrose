import { describe, expect, test } from "vitest";
import { compDict } from "../Functions.js";
import { makePath } from "../../shapes/Path.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import { PathBuilder } from "../../renderer/PathBuilder.js";
import Path, { toPathString } from "../../renderer/Path.js";
import { PathCmd, PathDataV } from "../../types/value.js";

const canvas = makeCanvas(800, 700);


describe("key-name equality", () => {
  test("each function's key and name should be equal", () => {
    for (const [name, func] of Object.entries(compDict)) {
      expect(name).toEqual(func.name);
    }
  });

  describe("join path variants", () => {
    const context = simpleContext('join path variants');

    const buildPath1 = (builder: PathBuilder) => {
      return builder
        .moveTo([0, 0])
        .lineTo([1, 1])
        .lineTo([2, 2]);
    };

    const buildPath2 = (builder: PathBuilder) => {
      return builder
        .moveTo([2, 2])
        .lineTo([3, 0])
        .quadraticCurveTo([1, 1], [0, 1]);
    };

    const buildPath3 = (builder: PathBuilder) => {
      return builder
        .moveTo([0, 1])
        .lineTo([1, 0]);
    }

    const buildPath4 = (builder: PathBuilder) => {
      return builder
        .moveTo([0, 0]);
    }

    const path1 = buildPath1(new PathBuilder()).getPath().contents as PathCmd<number>[];
    const path2 = buildPath2(new PathBuilder()).getPath().contents as PathCmd<number>[];
    const path3 = buildPath3(new PathBuilder()).getPath().contents as PathCmd<number>[];
    const path4 = buildPath4(new PathBuilder()).getPath().contents as PathCmd<number>[];

    const concat123 = buildPath3(buildPath2(buildPath1(new PathBuilder()))).getPath().contents as PathCmd<number>[];
    const connect123 = buildPath1(new PathBuilder())
      .lineTo([2, 2])
      .lineTo([3, 0])
      .quadraticCurveTo([1, 1], [0, 1])
      .lineTo([0, 1])
      .lineTo([1, 0])
      .getPath()
      .contents as PathCmd<number>[];
    const join123 = buildPath1(new PathBuilder())
      .lineTo([3, 0])
      .quadraticCurveTo([1, 1], [0, 1])
      .lineTo([1, 0])
      .getPath()
      .contents as PathCmd<number>[];

    test("concatenate paths simple", () => {
      expect(toPathString(concat123, canvas.size)).toEqual(toPathString(
        compDict.concatenatePaths.body(context, [path1, path2, path3])
          .value.contents as PathCmd<number>[],
        canvas.size
      ));
    });

    test("connect paths simple", () => {
      expect(toPathString(connect123, canvas.size)).toEqual(toPathString(
        compDict.connectPaths.body(context, "open", [path1, path2, path3])
          .value.contents as PathCmd<number>[],
        canvas.size
      ));
    });

    test("join paths simple", () => {
      expect(toPathString(join123, canvas.size)).toEqual(toPathString(
        compDict.joinPaths.body(context, [path1, path2, path3])
          .value.contents as PathCmd<number>[],
        canvas.size
      ));
    });


    const concat124 = buildPath4(buildPath2(buildPath1(new PathBuilder()))).getPath().contents as PathCmd<number>[];
    const connect124 = buildPath1(new PathBuilder())
      .lineTo([2, 2])
      .lineTo([3, 0])
      .quadraticCurveTo([1, 1], [0, 1])
      .lineTo([0, 0])
      .lineTo([0, 0])
      .getPath()
      .contents as PathCmd<number>[];
    const join124 = buildPath1(new PathBuilder())
      .lineTo([3, 0])
      .quadraticCurveTo([1, 1], [0, 1])
      .getPath()
      .contents as PathCmd<number>[];

    test("concatenate paths empty ending", () => {
      expect(toPathString(concat124, canvas.size)).toEqual(toPathString(
        compDict.concatenatePaths.body(context, [path1, path2, path4])
          .value.contents as PathCmd<number>[],
        canvas.size
      ));
    });

    test("connect paths empty ending", () => {
      expect(toPathString(connect124, canvas.size)).toEqual(toPathString(
        compDict.connectPaths.body(context, "closed", [path1, path2, path4])
          .value.contents as PathCmd<number>[],
        canvas.size
      ));
    });

    test("join paths empty ending", () => {
      expect(toPathString(join124, canvas.size)).toEqual(toPathString(
        compDict.joinPaths.body(context, [path1, path2, path4])
          .value.contents as PathCmd<number>[],
        canvas.size
      ));
    });
  });
});
