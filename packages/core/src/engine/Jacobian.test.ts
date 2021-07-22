import { numbered } from "compiler/Style";
import eig from "eigen";
import { eigObjToMatrix, getNullspaceBasisVectors } from "./Jacobian";

const r1 = [1, 2, 3];
const r2 = [4, 5, 6];
const r3 = [7, 8, 9];

const getPairs = <T>(arr: T[]): [T, T][] => {
  const pairsForElemAtIndexI = (i: number): [T, T][] => {
    // j!==i to avoid self comparison.
    // j<i excluded to avoid double-counting pairs,
    // i.e. we don't want both
    //	[<index 1 obj>, <index 2 obj>] and [<index 2 obj>, <index 1 obj>]
    return arr
      .filter((elem, j) => {
        return j > i;
      })
      .map((vec) => {
        return [vec, arr[i]];
      });
  };

  return arr.map((elem, index): [T, T][] => pairsForElemAtIndexI(index)).flat();
};

describe("misc", () => {
  test("floating pt vs int equality", () => {
    expect(0).toEqual(-0.0 || 0.0);
  });
  const t1 = [1, 2, 3];
  const res1: [number, number][] = [
    [2, 1],
    [3, 1],
    [3, 2],
  ];
  const t2 = [1, 2, 3, 4, 5, 6];
  const res2: [number, number][] = [
    [2, 1],
    [3, 1],
    [4, 1],
    [5, 1],
    [6, 1],
    [3, 2],
    [4, 2],
    [5, 2],
    [6, 2],
    [4, 3],
    [5, 3],
    [6, 3],
    [5, 4],
    [6, 4],
    [6, 5],
  ];
  test("getPairs t1", () => {
    const pairs = getPairs(t1);
    expect(res1.filter((elem) => pairs.includes(elem)).length).toBe(0);
  });
  test("getPairs t2", () => {
    const pairs = getPairs(t2);
    expect(res2.filter((elem) => pairs.includes(elem)).length).toBe(0);
  });
});

describe("eigObjToMatrix", () => {
  test("empty matrix", async () => {
    await eig.ready;
    const m1 = new eig.Matrix([]);
    expect(eigObjToMatrix(m1)).toEqual([]);
  });

  test("single col", async () => {
    await eig.ready;
    const m2 = new eig.Matrix([1, 2, 3]);
    expect(eigObjToMatrix(m2)).toEqual([[1], [2], [3]]);
  });

  test("3x3", async () => {
    await eig.ready;
    const m = new eig.Matrix([r1, r2, r3]);
    expect(eigObjToMatrix(m)).toEqual([
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9],
    ]);
  });
});

describe("numbered tests with matrices", () => {
  test("numbered rows", async () => {
    await eig.ready;
    const m = new eig.Matrix([r1, r2, r3]);
    expect(numbered(eigObjToMatrix(m))).toEqual([
      [[1, 2, 3], 0],
      [[4, 5, 6], 1],
      [[7, 8, 9], 2],
    ]);
  });

  test("numbered col", async () => {
    await eig.ready;
    const c = new eig.Matrix([1, 2, 3]);
    expect(numbered(eigObjToMatrix(c))).toEqual([
      [[1], 0],
      [[2], 1],
      [[3], 2],
    ]);
  });
});

describe("keenans example test", () => {
  const r1 = [
    2.49862,
    -1.08258,
    -2.49862,
    1.08258,
    0,
    0,
    -2.72306,
    -2.72306,
    0,
  ];
  const r2 = [0, 0, -0.524991, 2.9595, 0.524991, -2.9595, 0, -3.0057, -3.0057];
  const r3 = [1.97362, 1.87692, 0, 0, -1.97362, -1.87692, -2.7236, 0, -2.7236];
  test("getNullspaceBasisVectors test", async () => {
    await eig.ready;
    const J = new eig.Matrix([r1, r2, r3]);
    const A = J.transpose().matMul(J);
    const e = eig.Decompositions.svd(A, false);

    const evecs = eigObjToMatrix(e.V.transpose());

    const last6vecs = evecs.slice(3, evecs.length);

    const nspacevecs = getNullspaceBasisVectors([r1, r2, r3]);

    // console.log(nspacevecs)
    // console.log(nspacevecs.length, evecs.length)

    expect(
      nspacevecs.every((vec, index) => {
        const vec1 = new eig.Matrix(vec);
        const vec2 = new eig.Matrix(last6vecs[index]);
        const diff = vec1.matSub(vec2);
        const sqrNorm = diff.dot(diff);
        return Math.sqrt(sqrNorm) < 10e-6;
      })
    ).toBe(true);
  });

  test("checking validity of nullspace vectors, i.e. Jv === 0", async () => {
    await eig.ready;
    const J = new eig.Matrix([r1, r2, r3]);
    const nspacevecs = getNullspaceBasisVectors([r1, r2, r3]);

    expect(
      nspacevecs.every((vec) => {
        const vecObj = new eig.Matrix(vec);
        const res = J.matMul(vecObj);
        const sqrNorm = res.dot(res);
        return Math.sqrt(sqrNorm) < 10e-10;
      })
    ).toBe(true);
  });

  test("checking orthogonality of nullspace vectors", async () => {
    await eig.ready;
    const nspacevecs = getNullspaceBasisVectors([r1, r2, r3]);
    const vecPairs = getPairs(nspacevecs);
    expect(
      vecPairs.every(([v1, v2]) => {
        const [v1obj, v2obj] = [new eig.Matrix(v1), new eig.Matrix(v2)];
        return v1obj.dot(v2obj) < 10e-6;
      })
    ).toBe(true);
  });
});

// eig.GC.flush();
