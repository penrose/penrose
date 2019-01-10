import * as React from "react";
// TODO: lift state up to parent
// TODO: abstract out packet constructors so they can be consumed by dependents

interface IState {
  autostep: boolean;
}
interface IProps {
  converged: boolean;
  sendPacket(packet: string): void;
  download(): void;
}
class ButtonBar extends React.Component<IProps, IState> {
  public readonly state = { autostep: false };
  public autoStepToggle = () => {
    const packet = { tag: "Cmd", contents: { command: "autostep" } };
    this.setState({
      autostep: !this.state.autostep
    });
    this.props.sendPacket(JSON.stringify(packet));
  };
  public step = () => {
    const packet = { tag: "Cmd", contents: { command: "step" } };
    this.props.sendPacket(JSON.stringify(packet));
  };
  public resample = () => {
    const packet = { tag: "Cmd", contents: { command: "resample" } };
    this.props.sendPacket(JSON.stringify(packet));
  };
  public render() {
    const { autostep } = this.state;
    const { converged } = this.props;
    return (
      <div style={{ display: "flex", justifyContent: "middle" }}>
        <button onClick={this.autoStepToggle}>
          autostep {autostep ? "(on)" : "(off)"}
        </button>
        <button onClick={this.step}>step</button>
        <button onClick={this.resample}>resample</button>
        <button onClick={this.props.download}>download</button>
        <div
          style={{
            borderRadius: 100,
            display: "inline-block",
            width: 20,
            height: 20,
            backgroundColor: converged ? "#55de55" : "#ff9d23"
          }}
        />
      </div>
    );
  }
}

export default ButtonBar;
