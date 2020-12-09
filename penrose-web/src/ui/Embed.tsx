import { resample, stepState } from "API";
import * as React from "react";
import ButtonBar from "ui/ButtonBar";
import Canvas from "ui/Canvas";
import { converged } from "packets";

interface IEmbedState {
  data: State;
}

class Embed extends React.Component<any, IEmbedState> {
  buttons: React.RefObject<ButtonBar>;
  canvas: React.RefObject<Canvas>;

  constructor(props: IEmbedState) {
    super(props);
    this.state = { data: props.data };
    this.canvas = React.createRef<Canvas>();
    this.buttons = React.createRef<ButtonBar>();
  }

  public onCanvasState = async (canvasState: State) => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise((r) => setTimeout(r, 1));

    await this.setState({
      data: canvasState,
    });
    if (!converged(canvasState)) {
      await this.step();
    }
  };

  public step = async () => {
    const stepped = await stepState(this.state.data!);
    this.onCanvasState(stepped);
  };

  public resample = async () => {
    const NUM_SAMPLES = 1;
    const oldState = this.state.data;
    if (oldState) {
      const resampled = await resample(oldState, NUM_SAMPLES);
      this.onCanvasState(resampled);
    }
  };

  public updateData = async (data: any) => {
    await this.setState({ data: { ...data } });
    const stepped = await stepState(data);
    this.onCanvasState(stepped);
  };

  public render() {
    const { data } = this.state;
    return (
      <div
        className="penrose-embed"
        style={{
          height: "100%",
          display: "flex",
          flexFlow: "column",
          overflow: "hidden",
        }}
      >
        <div style={{ flexShrink: 0 }}>
          <ButtonBar
            step={this.step}
            showInspector={false}
            autostep
            converged
            initial={false}
            resample={this.resample}
            ref={this.buttons}
          />
        </div>
        <div style={{ flexGrow: 1, position: "relative", overflow: "hidden" }}>
          <Canvas data={data} updateData={this.updateData} lock={false} />
        </div>
      </div>
    );
  }
}

export default Embed;
