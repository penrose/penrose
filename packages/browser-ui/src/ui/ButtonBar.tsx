import * as React from "react";
import { FileSocketResult } from "./FileSocket";

interface IProps {
  converged: boolean;
  autostep: boolean;
  initial: boolean;
  showInspector: boolean;
  files: FileSocketResult | null;
  connected: boolean;

  toggleInspector?(): void;
  downloadPDF?(): void;

  downloadSVG?(): void;
  downloadState?(): void;
  autoStepToggle?(): void;
  step(): void;
  stepUntilConvergence(): void;
  resample(): void;
  reconnect(): void;
}
class ButtonBar extends React.Component<IProps> {
  public render() {
    const {
      converged,
      initial,
      autostep,
      autoStepToggle,
      downloadPDF,
      downloadSVG,
      downloadState,
      step,
      stepUntilConvergence,
      resample,
      toggleInspector,
      showInspector,
      files,
      connected,
      reconnect,
    } = this.props;
    return (
      <div style={{ display: "flex", justifyContent: "middle" }}>
        {autoStepToggle && (
          <button onClick={autoStepToggle}>
            autostep {autostep ? "(on)" : "(off)"}
          </button>
        )}
        <button onClick={step}>step</button>
        <button onClick={stepUntilConvergence}>step until convergence</button>
        <button
          onClick={resample}
          disabled={!converged && !initial && autostep}
        >
          resample
        </button>
        {downloadPDF && <button onClick={downloadPDF}>download PDF</button>}
        {downloadSVG && <button onClick={downloadSVG}>download SVG</button>}
        {downloadState && (
          <button onClick={downloadState}>download State</button>
        )}
        {toggleInspector && (
          <button onClick={toggleInspector}>
            {showInspector ? "hide" : "show"} inspector
          </button>
        )}

        <div
          style={{
            borderRadius: 100,
            display: "inline-block",
            width: 20,
            height: 20,
            backgroundColor:
              converged || !autostep
                ? "#55de55"
                : initial
                ? "#4286f4"
                : "#ff9d23",
          }}
        />
        <div
          style={{
            display: "inline-block",
            marginLeft: "1em",
            color: "#303030",
            fontSize: "14px",
          }}
        >
          {files === null
            ? "no files received from server"
            : `sub: ${files.substance.fileName} sty: ${files.style.fileName} dsl: ${files.domain.fileName}`}
        </div>
        <button onClick={reconnect} disabled={connected}>
          {connected ? "connected" : "reconnect"}
        </button>
      </div>
    );
  }
}

export default ButtonBar;
