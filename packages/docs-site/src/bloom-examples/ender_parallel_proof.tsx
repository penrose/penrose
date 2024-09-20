import {
  DiagramBuilder,
  Renderer,
  Vec2,
  VecN,
  arc,
  atan2,
  canvas,
  constraints,
  cross2D,
  fromHomogeneous,
  ifCond,
  neg,
  ops,
  rotate,
  toHomogeneous,
  translate,
  useDiagram,
  vdist,
} from "@penrose/bloom";
import { gt } from "@penrose/core";

const buildInscribedAngles = async () => {
  const db = new DiagramBuilder(canvas(400, 400), "");

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
    layer,
  } = db;

  const Circle = type();
  const Angle = type();
  const Point = type();

  const AngleHasPoints = predicate();
  const CircleHasCenter = predicate();
  const DraggableOnCircle = predicate();
  const Faint = predicate();

  const MRad = 100;

  // create points and positions
  const M = Point();
  M.pos = [50, 0];

  const A = Point();
  A.pos = [input({ init: -50 }), input({ init: 50 })];

  const B = Point();
  B.pos = [M.pos[0] - MRad, 0];

  const C = Point();
  C.pos = [input({ init: M.pos[0] + MRad }), input({ init: 0 })];

  const circM = Circle();
  CircleHasCenter(circM, M);
  circM.rad = MRad;

  const AMB = Angle();
  AngleHasPoints(AMB, A, M, B);
  AMB.color = [0, 0, 1, 1];

  const ACB = Angle();
  AngleHasPoints(ACB, A, C, B);
  ACB.color = [0, 0.7, 0, 1];

  // create transformation from bold circle to faint circle
  const transform = (v: VecN) => {
    const hv = toHomogeneous(v);
    const bc = ops.vsub(C.pos, B.pos);
    const cm = ops.vsub(M.pos, C.pos);
    const theta = atan2(bc[0], bc[1]);
    const hvp = ops.mvmul(
      rotate(neg(theta), M.pos[0], M.pos[1]),
      ops.mvmul(translate(cm[0], cm[1]), hv),
    );
    return fromHomogeneous(hvp);
  };

  // create maps of points to faint circle
  const ApPos = transform(A.pos);
  const BpPos = transform(B.pos);
  const CpPos = transform(C.pos);
  const MpPos = transform(M.pos);

  const Mp = Point();
  Mp.pos = MpPos;

  const circMp = Circle();
  CircleHasCenter(circMp, Mp);
  circMp.rad = MRad;
  Faint(circMp);

  const Ap = Point();
  Ap.pos = ApPos;

  const Bp = Point();
  Bp.pos = BpPos;

  const Cp = Point();
  Cp.pos = CpPos;

  const ApCpBp = Angle();
  AngleHasPoints(ApCpBp, Ap, Cp, Bp);
  ApCpBp.color = [0, 0.7, 0, 0.5];
  Faint(ApCpBp);

  const ApMpBp = Angle();
  AngleHasPoints(ApMpBp, Ap, Mp, Bp);
  ApMpBp.color = [0, 0, 1, 0.5];
  Faint(ApMpBp);

  DraggableOnCircle(A, circM);
  DraggableOnCircle(C, circM);

  forall({ p: Point }, ({ p }) => {
    // pass for now, maybe text later
  });

  // draw circles
  forallWhere(
    { c: Circle, p: Point },
    ({ c, p }) => CircleHasCenter.test(c, p),
    ({ c, p }) => {
      c.icon = circle({
        r: c.rad,
        strokeColor: c.color ? c.color : [0, 0, 0, 1],
        strokeWidth: Faint.test(c) ? 1.5 : 3,
        fillColor: [0, 0, 0, 0],
        center: p.pos,
        ensureOnCanvas: false,
      });
    },
  );

  // draw draggable point handles
  forallWhere(
    { p: Point, c: Circle },
    ({ p, c }) => DraggableOnCircle.test(p, c),
    ({ p, c }) => {
      p.handle = circle({
        center: p.pos,
        r: 20,
        fillColor: [0, 0, 0, 0.1],
        drag: true,
        // project dragged position onto circle
        dragConstraint: ([x, y]) => {
          const [cx, cy] = [
            c.icon.center[0] as number,
            c.icon.center[1] as number,
          ];
          const [dx, dy] = [x - cx, y - cy];
          const norm = Math.sqrt(dx * dx + dy * dy);
          const [nx, ny] = [dx / norm, dy / norm];
          return [cx + nx * c.rad, cy + ny * c.rad];
        },
        ensureOnCanvas: false,
      });

      // ensure handle starts on circle
      ensure(constraints.equal(vdist(p.pos, c.icon.center), c.rad));
    },
  );

  // draw angles
  forallWhere(
    { a: Angle, p1: Point, p2: Point, p3: Point },
    ({ a, p1, p2, p3 }) => AngleHasPoints.test(a, p1, p2, p3),
    ({ a, p1, p2, p3 }) => {
      a.line1 = line({
        start: p2.pos,
        end: p1.pos,
        strokeColor: a.color ? a.color : [0, 0, 0, 1],
        strokeWidth: Faint.test(a) ? 1.5 : 3,
        ensureOnCanvas: false,
        strokeLinecap: "round",
      });

      a.line2 = line({
        start: p2.pos,
        end: p3.pos,
        strokeColor: a.color ? a.color : [0, 0, 0, 1],
        strokeWidth: Faint.test(a) ? 1.5 : 3,
        ensureOnCanvas: false,
        strokeLinecap: "round",
      });
    },
  );

  layer(ApMpBp.line2, circM.icon);

  return await build();
};

const buildVerticalAngles = async () => {
  const db = new DiagramBuilder(canvas(400, 400), "");

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
    layer,
    path,
    bindToInput,
  } = db;

  const Angle = type();
  const LineSegment = type();
  const Point = type();

  const HasPoints = predicate();
  const DraggableAround = predicate();
  const AnglePair = predicate();

  const len = 200;

  const M = Point();
  M.pos = [0, 0];

  const A = Point();
  DraggableAround(A, M);

  const B = Point();
  DraggableAround(B, M);

  const C = Point();
  DraggableAround(C, M);

  const D = Point();
  DraggableAround(D, M);

  const AB = LineSegment();
  HasPoints(AB, A, M, B);

  const CD = LineSegment();
  HasPoints(CD, C, M, D);

  const AMC = Angle();
  HasPoints(AMC, A, M, C);
  AMC.color = [0.7, 0, 0, 1];

  const BMD = Angle();
  HasPoints(BMD, B, M, D);
  BMD.color = [0.7, 0, 0, 1];

  AnglePair(AMC, BMD);

  const AMD = Angle();
  HasPoints(AMD, A, M, D);
  AMD.color = [0, 0, 0.7, 1];

  const BMC = Angle();
  HasPoints(BMC, B, M, C);
  BMC.color = [0, 0, 0.7, 1];

  AnglePair(AMD, BMC);

  forall({ p: Point }, ({ p }) => {
    // if no position is specified, make in optimized
    if (!p.pos) {
      p.pos = [input(), input()];
    }
  });

  forallWhere(
    { p: Point, m: Point },
    ({ p, m }) => DraggableAround.test(p, m),
    ({ p, m }) => {
      p.icon = circle({
        center: p.pos,
        r: 20,
        fillColor: [0, 0, 0, 0.1],
        drag: true,
        dragConstraint: ([x, y]) => {
          const [cx, cy] = m.pos as [number, number];
          const [dx, dy] = [x - cx, y - cy];
          const norm = Math.sqrt(dx * dx + dy * dy);
          const [nx, ny] = [dx / norm, dy / norm];
          return [cx + (nx * len) / 2, cy + (ny * len) / 2];
        },
      });
    },
  );

  forallWhere(
    { l: LineSegment, p1: Point, m: Point, p2: Point },
    ({ l, p1, m, p2 }) => HasPoints.test(l, p1, m, p2),
    ({ l, p1, m, p2 }) => {
      // create two points that _must_ have m as a midpoint (not just an
      // optimizer constraint), and ensure they match the handles

      const dir = [input(), input()];
      const p1p = ops.vadd(
        m.pos,
        ops.vmul(len / 2, ops.vnormalize(dir)),
      ) as Vec2;
      const p2p = ops.vadd(
        m.pos,
        ops.vmul(-len / 2, ops.vnormalize(dir)),
      ) as Vec2;

      l.icon = line({
        start: p1p,
        end: p2p,
        strokeColor: [0, 0, 0, 1],
        strokeWidth: 2,
        strokeLinecap: "round",
      });

      ensure(constraints.equal(vdist(p1p, p1.pos), 0));
      ensure(constraints.equal(vdist(p2p, p2.pos), 0));
    },
  );

  // make different angle pairs have different radius indicators
  forallWhere(
    { a: Angle, b: Angle },
    ({ a, b }) => AnglePair.test(a, b),
    ({ a, b }, i) => {
      a.rad = (i + 1) * 10;
      b.rad = a.rad;
    },
  );

  // draw angle paths
  // might be simpler to do with a clipped circle
  forallWhere(
    { a: Angle, p1: Point, p2: Point, p3: Point },
    ({ a, p1, p2, p3 }) => HasPoints.test(a, p1, p2, p3),
    ({ a, p1, p2, p3 }) => {
      a.rad = a.rad ? a.rad : 10;

      // a.rad from p2 towards p1
      const a1: Vec2 = ops.vadd(
        ops.vmul(a.rad, ops.vnormalize(ops.vsub(p2.pos, p1.pos))),
        p2.pos,
      ) as Vec2;

      // a.rad from p2 towards p3
      const a2: Vec2 = ops.vadd(
        ops.vmul(a.rad, ops.vnormalize(ops.vsub(p2.pos, p3.pos))),
        p2.pos,
      ) as Vec2;

      // find direction to draw path
      const dir = ifCond(
        gt(
          cross2D(
            ops.vsub(p1.pos, p2.pos) as Vec2,
            ops.vsub(p3.pos, p2.pos) as Vec2,
          ),
          0,
        ),
        0,
        1,
      );

      // draw path
      a.icon = path({
        d: arc("open", a1, a2, [a.rad, a.rad], 0, 0, dir),
        strokeColor: a.color,
        ensureOnCanvas: false,
      });
    },
  );

  return await build();
};

export default function () {
  const inscribedAngles = useDiagram(buildInscribedAngles);
  const verticalAngles = useDiagram(buildVerticalAngles);
  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
      }}
    >
      <div
        style={{
          width: "50%",
        }}
      >
        <Renderer diagram={inscribedAngles} />
      </div>
      <div
        style={{
          width: "50%",
        }}
      >
        <Renderer diagram={verticalAngles} />
      </div>
    </div>
  );
}
