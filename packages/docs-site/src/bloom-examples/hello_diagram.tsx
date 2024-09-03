import {
  DiagramBuilder,
  Renderer,
  Vec2,
  canvas,
  constraints,
  ops,
  useDiagram,
} from "@penrose/bloom";

const buildMyDiagram = async () => {
  const db = new DiagramBuilder(canvas(400, 200), "", 1);

  const { type, predicate, circle, line, build, forall, forallWhere, ensure } =
    db;

  const Point = type();
  const Arrow = type();
  const Connects = predicate();

  const p1 = Point();
  const p2 = Point();
  const arrow = Arrow();
  Connects(arrow, p1, p2);

  const pointRad = 30;
  const pointMargin = 10;

  forall({ p: Point }, ({ p }) => {
    p.icon = circle({
      r: pointRad,
      drag: true,
    });
  });

  forallWhere(
    { a: Arrow, p: Point, q: Point },
    ({ a, p, q }) => Connects.test(a, p, q),
    ({ a, p, q }) => {
      const pq = ops.vsub(q.icon.center, p.icon.center); // vector from p to q
      const pqNorm = ops.vnormalize(pq); // direction from p to q
      const pStart = ops.vmul(pointRad + pointMargin, pqNorm); // vector from p to line start
      const start = ops.vadd(p.icon.center, pStart); // line start
      const end = ops.vsub(q.icon.center, pStart); // line end

      a.icon = line({
        start: start as Vec2,
        end: end as Vec2,
        endArrowhead: "straight",
      });

      ensure(
        constraints.greaterThan(
          ops.vdist(p.icon.center, q.icon.center),
          2 * (pointRad + pointMargin) + 20,
        ),
      );
    },
  );

  return await build();
};

export const MyDiagramComponent = () => {
  const diagram = useDiagram(buildMyDiagram);
  return <Renderer diagram={diagram} />;
};

export default MyDiagramComponent;
