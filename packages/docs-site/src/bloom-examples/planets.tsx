import { canvas, DiagramBuilder, Renderer, useDiagram } from "@penrose/bloom";

const buildDiagram = async () => {
  const db = new DiagramBuilder(canvas(400, 400), "");
  const { build, circle, type, predicate, forallWhere } = db;

  const Sun = type();
  const Planet = type();
  const Orbit = predicate();

  const sun = Sun();

  const mercury = Planet();
  Orbit(mercury, 88, 58);
  mercury.color = [1, 0.9, 0, 1];

  const venus = Planet();
  Orbit(venus, 225, 107);
  venus.color = [0.9, 0.7, 0.5, 1];

  const earth = Planet();
  Orbit(earth, 365, 150);
  earth.color = [0, 0.5, 1, 1];

  const mars = Planet();
  Orbit(mars, 687, 228);
  mars.color = [1, 0, 0, 1];

  sun.icon = circle({
    r: 20,
    fillColor: [1, 1, 0, 1],
    center: [0, 0],
  });

  return await build();
};

export default function Planets() {
  const diagram = useDiagram(buildDiagram);
  return <Renderer diagram={diagram} />;
}
