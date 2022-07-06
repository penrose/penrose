import * as fn from "contrib/Functions";
import { primaryGraph } from "engine/Autodiff";
import { fuzz, fuzzSetup, isReal } from "utils/NumFuzz";
import {
  fnInradius,
  fnSdfEllipseAsNums,
  fnSignedDistanceCircle,
  fnSignedDistanceEllipse,
} from "utils/NumFuzzConfig";

/**
 * Test suite for the NumFuzz package
 */
describe("fuzzFunctions", () => {
  /**
   * Fuzz signedDistance using a Circle w/varying center
   */
  test("fnSignedDistanceCircle", () => {
    const fe = fuzzSetup({
      fnCfg: fnSignedDistanceCircle,
      graphFn: primaryGraph,
      numTests: 10000,
      oracleFn: isReal,
      outputFile: "fuzzedGen.txt",
    });
    const fuzzResults = fuzz(
      fn.compDict.signedDistance(
        fe.context!,
        [fe.shape!.shapeType, fe.shape!],
        fe.nonShapeV.p
      ).contents,
      fe
    );
    expect(fuzzResults.filter((e) => !e.passed).length).toBe(0);
  });

  /**
   * Fuzz sdfEllipseAsNums using an Ellipse w/varying rx, ry, center, and p.
   */
  // Skip: SDF for Ellipse is not working yet
  test.skip("fuzzSdfEllipseAsNums", () => {
    const fe = fuzzSetup({
      fnCfg: fnSdfEllipseAsNums,
      graphFn: primaryGraph,
      numTests: 10000,
      oracleFn: isReal,
      outputFile: "fuzzedGen.txt",
    });
    const fuzzResults = fuzz(
      fn.exportsForTestingOrAnalysis.sdEllipseAsNums(
        fe.nonShape.radiusx,
        fe.nonShape.radiusy,
        [fe.nonShape.centerx, fe.nonShape.centery],
        [fe.nonShape.px, fe.nonShape.py]
      ),
      fe
    );
    expect(fuzzResults.filter((e) => !e.passed).length).toBe(0);
  });

  /**
   * Fuzz signedDistance using an Ellipse w/varying rx, ry, center, and p.
   */
  // Skip: SDF for Ellipse is not working yet
  test.skip("fuzzSignedDistanceEllipse", () => {
    const fe = fuzzSetup({
      fnCfg: fnSignedDistanceEllipse,
      graphFn: primaryGraph,
      numTests: 10000,
      oracleFn: isReal,
      outputFile: "fuzzedGen.txt",
    });
    const fuzzResults = fuzz(
      fn.compDict.signedDistance(
        fe.context,
        [fe.shape!.shapeType, fe.shape!],
        [fe.nonShape.px, fe.nonShape.py]
      ).contents,
      fe
    );
    expect(fuzzResults.filter((e) => !e.passed).length).toBe(0);
  });

  /**
   * Fuzz signedDistance using an Ellipse w/varying rx, ry, center, and p.
   */
  test("fuzzInradius", () => {
    const fe = fuzzSetup({
      fnCfg: fnInradius,
      graphFn: primaryGraph,
      numTests: 10000,
      oracleFn: isReal,
      outputFile: "fuzzedGen.txt",
    });
    const fuzzResults = fuzz(
      fn.compDict.inradius(
        fe.context,
        [fe.nonShape.px, fe.nonShape.py],
        [fe.nonShape.qx, fe.nonShape.qy],
        [fe.nonShape.rx, fe.nonShape.ry]
      ).contents,
      fe
    );
    expect(fuzzResults.filter((e) => !e.passed).length).toBe(0);
  });
});
