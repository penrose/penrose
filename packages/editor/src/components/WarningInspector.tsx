import { StyleWarning } from "@penrose/core/dist/types/errors";
import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms";

import { NodeType, SourceRange } from "@penrose/core/dist/types/ast";

const locc = (nodeType: NodeType, node: SourceRange): string => {
  return `line ${node.start.line}, column ${
    node.start.col + 1
  } of ${nodeType} program`;
};

const warningDivStyle = {
  bottom: "0px",
  backgroundColor: "rgb(255, 218, 218)",
  maxHeight: "100%",
  maxWidth: "100%",
  minHeight: "100px",
  overflow: "auto",
  padding: "5px",
};

const warningHeaderStyle = {
  fontWeight: "bold",
  color: "rgb(238, 78, 78)",
  fontSize: "14px",
};

// NOTE: using index just for unique keys
// TODO: figure out key naming conventions
function FormatWarning(warning: StyleWarning, index: number) {
  switch (warning.tag) {
    case "NoopDeleteWarning":
      return (
        <div style={warningDivStyle}>
          <span style={warningHeaderStyle}> warning (NoopDeleteWarning) </span>
          <p key={`${warning.tag},${index}`}>
            {" "}
            Deleting nonexistent '{warning.path.name}' <br></br>
            (at {locc("Style", warning.path)}).{" "}
          </p>
        </div>
      );
    case "ImplicitOverrideWarning":
      return (
        <div style={warningDivStyle}>
          <span style={warningHeaderStyle}>
            {" "}
            warning (ImplicitOverrideWarning){" "}
          </span>
          <p key={`${warning.tag},${index}`}>
            Implicitly overriding '{warning.path.name}' <br></br>
            (at {locc("Style", warning.path)}).{" "}
          </p>
        </div>
      );
    case "LayerCycleWarning":
      return (
        <div style={warningDivStyle}>
          <span style={warningHeaderStyle}> warning (LayerCycleWarning) </span>
          <p key={`${warning.tag},${index}`}>
            Cycles detected in layering order:{""} <br></br>
            {warning.cycles.map((c) => c.join(" ➔ "))}. <br></br>
            The system approximated a global layering order instead:<br></br>
            {warning.approxOrdering.join(" ➔ ")};
          </p>
        </div>
      );
  }
}

export default function WarningInspector() {
  const { state } = useRecoilValue(diagramState);
  // NOTE: can only get one error at a time currently,
  // and right now it's being displayed in the diagram potion.
  // Would likely need some refactoring to display all errors

  let warnings = state?.warnings || [];

  return (
    <div key={`${warnings.toString()}`}>
      {warnings.map((warning, index) => FormatWarning(warning, index))}
    </div>
  );
}
