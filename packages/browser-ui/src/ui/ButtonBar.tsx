import * as React from "react";
import { FileSocketResult } from "./FileSocket";

interface Props {
  converged: boolean;
  autostep: boolean;
  initial: boolean;
  error: boolean;
  showInspector: boolean;
  files: FileSocketResult | undefined;
  connected: boolean;

  toggleInspector?(): void;
  downloadPDF?(): void;

  downloadSVG?(): void;
  downloadState?(): void;
  autoStepToggle?(): void;
  step(numSteps: number): void;
  stepUntilConvergence(): void;
  reset(): void;
  resample(): void;
  reconnect(): void;
}
class ButtonBar extends React.Component<Props> {
  public render(): JSX.Element {
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
      reset,
      resample,
      toggleInspector,
      showInspector,
      files,
      connected,
      reconnect,
      error,
    } = this.props;
    return (
      <div style={{ display: "flex", justifyContent: "middle" }}>
        {autoStepToggle && (
          <button onClick={autoStepToggle}>
            autostep {autostep ? "(on)" : "(off)"}
          </button>
        )}
        <button onClick={() => step(1)}>x1 optimization step</button>
        <button onClick={stepUntilConvergence}>step until convergence</button>
        <button
          onClick={reset}
          disabled={!converged && !initial && !error && autostep}
        >
          reset
        </button>
        <button
          onClick={resample}
          disabled={!converged && !initial && !error && autostep}
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
          {files === undefined
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
