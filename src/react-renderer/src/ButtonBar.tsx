import * as React from "react";
import {ILayer} from "./types";

interface IProps {
  converged: boolean;
  autostep: boolean;
  layers: ILayer[];

  downloadPDF(): void;

  downloadSVG(): void;
  autoStepToggle(): void;
  step(): void;
  resample(): void;

  toggleLayer(layer: string): void;
}
class ButtonBar extends React.Component<IProps> {
  public render() {
    const {
      converged,
      autostep,
      autoStepToggle,
      downloadPDF,
      downloadSVG,
      step,
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
        <button onClick={resample}>resample</button>
        <button onClick={downloadPDF}>download PDF</button>
        <button onClick={downloadSVG}>download SVG</button>
        {layers.map(({layer, enabled}: ILayer, key: number) => {
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
            backgroundColor: converged || !autostep ? "#55de55" : "#ff9d23"
          }}
        />
      </div>
    );
  }
}

export default ButtonBar;
