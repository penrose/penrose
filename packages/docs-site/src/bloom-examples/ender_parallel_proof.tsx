import {
  DiagramBuilder,
  Renderer,
  canvas,
  constraints,
  norm,
  ops,
  useDiagram,
  vdist,
} from "@penrose/bloom";
import { useCallback, useState } from "react";

const buildDiagram = async (
  variation: string,
  enabledSegments: Set<string>,
) => {
  const db = new DiagramBuilder(canvas(400, 400), variation);

  const {
    type,
    predicate,
    circle,
    line,
    build,
    forall,
    forallWhere,
    ensure,
    input,
    rectangle,
    text,
    layer,
    path,
    equation,
    encourage,
  } = db;

  const LineSegment = type();
  const Point = type();
  const Triangle = type();
  const Angle = type();

  const AddTo = predicate();
  const IsBetween = predicate();
  const Congruent = predicate();

  const A = Point();
  A.label = "A";
  const B = Point();
  B.label = "B";
  const C = Point();
  C.label = "C";
  const D = Point();
  D.label = "D";
  const M = Point();
  M.label = "M";

  const AM = LineSegment();
  IsBetween(AM, A, M);
  AM.label = "AM";
  const BM = LineSegment();
  IsBetween(BM, B, M);
  BM.label = "BM";
  const CM = LineSegment();
  IsBetween(CM, C, M);
  CM.label = "CM";
  const DM = LineSegment();
  IsBetween(DM, D, M);
  DM.label = "DM";
  const AB = LineSegment();
  IsBetween(AB, A, B);
  AB.label = "AB";
  const CD = LineSegment();
  IsBetween(CD, C, D);
  CD.label = "CD";
  const AC = LineSegment();
  IsBetween(AC, A, C);
  AC.label = "AC";
  const BD = LineSegment();
  IsBetween(BD, B, D);
  BD.label = "BD";

  const ACM = Triangle();
  IsBetween(ACM, A, C, M);
  ACM.label = "ACM";
  const BDM = Triangle();
  IsBetween(BDM, B, D, M);
  BDM.label = "BDM";

  AddTo(AM, BM, AB);
  AddTo(CM, DM, CD);

  Congruent(AM, BM);
  Congruent(CM, DM);

  forall({ p: Point }, ({ p }) => {
    p.pos = [input(), input()];
    p.text = equation({
      string: p.label,
    });
  });

  forallWhere(
    { l: LineSegment, p1: Point, p2: Point },
    ({ l, p1, p2 }) => IsBetween.test(l, p1, p2),
    ({ l, p1, p2 }) => {
      l.icon = line({
        start: p1.pos,
        end: p2.pos,
      });

      if (enabledSegments.has(l.label)) {
        // bright blue thicker
        l.highlight = line({
          start: p1.pos,
          end: p2.pos,
          strokeColor: [0, 0, 1, 1],
          strokeWidth: 5,
        });

        l.label = equation({
          string: l.label,
        });

        const midpoint = ops.vdiv(ops.vadd(p1.pos, p2.pos), 2);
        ensure(constraints.equal(10, vdist(midpoint, l.label.center)));
      }
    },
  );

  forallWhere(
    { l: LineSegment, l1: LineSegment, l2: LineSegment },
    ({ l1, l2, l }) => AddTo.test(l1, l2, l),
    ({ l1, l2, l }) => {
      l.icon.strokeColor[3] = 0;
      let points;
      if (l1.icon.start === l2.icon.start) {
        points = [l1.icon.end, l1.icon.start, l2.icon.end];
      } else if (l1.icon.start === l2.icon.end) {
        points = [l1.icon.end, l1.icon.start, l2.icon.start];
      } else if (l1.icon.end === l2.icon.start) {
        points = [l1.icon.start, l1.icon.end, l2.icon.end];
      } else if (l1.icon.end === l2.icon.end) {
        points = [l1.icon.start, l1.icon.end, l2.icon.start];
      } else {
        throw new Error("Lines are not connected");
      }
      ensure(constraints.collinearOrdered(points[0], points[1], points[2]));
    },
  );

  forallWhere(
    { l1: LineSegment, l2: LineSegment },
    ({ l1, l2 }) => Congruent.test(l1, l2),
    ({ l1, l2 }) => {
      ensure(
        constraints.equal(
          norm(ops.vsub(l1.icon.start, l1.icon.end)),
          norm(ops.vsub(l2.icon.start, l2.icon.end)),
        ),
      );
    },
  );

  forall({ p: Point, l: LineSegment }, ({ p, l }) => {
    ensure(constraints.disjoint(p.text, l.icon, 5));
    ensure(constraints.lessThan(norm(ops.vsub(p.pos, p.text.center)), 15));
  });

  return await build();
};

export default function () {
  const [variation, setVariation] = useState("16");
  const diagram = useDiagram(
    useCallback(() => buildDiagram(variation, new Set(["BM"])), [variation]),
  );
  return (
    <>
      <button
        onClick={() =>
          setVariation((v) => {
            const vp = String(Number.parseInt(v) + 1);
            console.log(vp);
            return vp;
          })
        }
      >
        Update
      </button>
      <Renderer diagram={diagram} />
    </>
  );
}
