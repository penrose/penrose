import { canvas, DiagramBuilder, Renderer, useDiagram } from "@penrose/bloom";

const buildDiagram = async () => {
  const db = new DiagramBuilder(canvas(400, 300), "asdf", 0);

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
  } = db;

  const Input = type();
  const Procedure = type();
  const Output = type();

  const inputs = Array.from({ length: 10 }, () => Input());
  const procedure = Procedure();
  const output = Output();

  procedure.icon = rectangle({
    width: 100,
    height: 100,
    fillColor: [1, 0.8, 0.9, 1],
    strokeColor: [0, 0, 0, 1],
    strokeWidth: 1,
    center: [0, 0],
  });
  procedure.text = text({
    string: "WebAssembly",
    center: [0, 0],
  });

  const inputX = -150;
  const inputDY = 20;
  forall({ i: Input }, ({ i }, j) => {
    i.icon = circle({
      r: 5,
      fillColor: [0.7, 0.9, 0, 1],
      strokeColor: [0, 0, 0, 1],
      strokeWidth: 1,
      center: [inputX, (j - (inputs.length - 1) / 2) * inputDY],
    });

    i.line = line({
      start: i.icon.center,
      end: [0, 0],
      endArrowhead: "straight",
    });

    layer(i.line, procedure.icon);
  });

  text({
    string: "Inputs",
    center: [inputX, inputs[0].icon.center[1] - 2 * inputDY],
  });

  output.text = text({
    string: "Diagram",
    center: [150, 0],
  });
  output.arrow = line({
    start: [60, 0],
    end: [output.text.center[0] - 30, 0],
    endArrowhead: "straight",
  });

  return await build();
};

export default function ProceduralDiagram() {
  const diagram = useDiagram(buildDiagram);
  return <Renderer diagram={diagram} />;
}
