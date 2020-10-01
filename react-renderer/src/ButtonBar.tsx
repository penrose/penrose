import * as React from "react";
import { ILayer } from "./types";

interface IProps {
  converged: boolean;
  autostep: boolean;
  initial: boolean;
  layers: ILayer[];

  downloadPDF(): void;

  downloadSVG(): void;
  autoStepToggle(): void;
  step(): void;
  stepUntilConvergence(): void;
  resample(): void;

  toggleLayer(layer: string): void;
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
      step,
      stepUntilConvergence,
      resample,
      toggleLayer,
      layers
    } = this.props;
    return (
      <div style={{ display: "flex", justifyContent: "middle" }}>
        <button onClick={autoStepToggle}>
          autostep {autostep ? "(on)" : "(off)"}
        </button>
        <button onClick={step}>step</button>
        <button onClick={stepUntilConvergence}>step until convergence</button>
        <button
          onClick={resample}
          disabled={!converged && !initial && autostep}
        >
          resample
        </button>
        <button onClick={downloadPDF}>download PDF</button>
        <button onClick={downloadSVG}>download SVG</button>
        {layers.map(({ layer, enabled }: ILayer, key: number) => {
          return (
            <button onClick={() => toggleLayer(layer)} key={key}>
              {layer} {enabled ? "(on)" : "(off)"}
            </button>
          );
        })}

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
                  : "#ff9d23"
          }}
        />
      </div>
    );
  }
}

export default ButtonBar;
