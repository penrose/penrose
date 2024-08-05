import { DiagramBuilder } from "bloom/lib/core/builder.js";
import { canvas } from "bloom/lib/core/utils.js";

const raysDiagram = async () => {
  const {
    type,
    predicate,
    forall,
    forallWhere,
    circle,
    line,
    build,
    layer,
    encourage,
    ensure,
    input,
  } = new DiagramBuilder(canvas(400, 400), "");

  return await build();
};

export default function RaysComponent() {
  return <></>;
}
