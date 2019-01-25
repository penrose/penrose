import * as React from "react";

interface IProps {
  converged: boolean;
  autostep: boolean;

  download(): void;
  autoStepToggle(): void;
  step(): void;
  resample(): void;
}
class ButtonBar extends React.Component<IProps> {
  public render() {
    const { converged, autostep, autoStepToggle, download, step, resample } = this.props;
    return (
      <div style={{ display: "flex", justifyContent: "middle" }}>
        <button onClick={autoStepToggle}>
          autostep {autostep ? "(on)" : "(off)"}
        </button>
        <button onClick={step}>step</button>
        <button onClick={resample}>resample</button>
        <button onClick={download}>download</button>
        <div
          style={{
            borderRadius: 100,
            display: "inline-block",
            width: 20,
            height: 20,
            backgroundColor: (converged || !autostep) ? "#55de55" : "#ff9d23"
          }}
        />
      </div>
    );
  }
}

export default ButtonBar;
