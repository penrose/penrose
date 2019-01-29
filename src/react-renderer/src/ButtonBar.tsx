import * as React from "react";
import {ILayers} from "./Canvas";

interface IProps {
  converged: boolean;
  autostep: boolean;
  layers: ILayers;

  download(): void;
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
      download,
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
        <button onClick={download}>download</button>
        {Object.keys(layers).map((layer: string, key: number) => {
          return (
            <button onClick={() => toggleLayer(layer)} key={key}>
              polygon layer {layers[layer] ? "(on)" : "(off)"}
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
