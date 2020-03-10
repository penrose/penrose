import * as React from "react";
import { Popout } from "react-popout-component";

import { IRendererEvents, IInstanceMap } from "../Protocol";
import Timeline from "./Timeline";
import Frames from "./views/Frames";

interface IProps {
  show: boolean;
  onClose(): void;
}

interface IState {
  instances: IInstanceMap;
  selectedInstance: string;
  selectedInstanceFrame: number;
}

class Inspector extends React.Component<IProps, IState> {
  public readonly state = {
    instances: {},
    selectedInstance: "",
    selectedInstanceFrame: -1
  };

  public readonly Events: IRendererEvents = {
    kind: "renderer",
    onConnectionStatus: console.log,
    onVersion: console.log,
    onCanvasState: (canvasState: any, id: string) => {
      //   const { selectedInstance, instances } = this.state;
      //   let instance = instances[id] || [];
      //   instance = [...instance, canvasState];
      //   this.setState({
      //     selectedInstance: selectedInstance === "" ? id : selectedInstance,
      //     instances: { ...instances, [id]: instance }
      //   });
    },
    onError: console.log
  };
  public selectInstanceFrame = (frame: number) => {
    this.setState({
      selectedInstanceFrame:
        frame === this.state.selectedInstanceFrame ? -1 : frame
    });
  };
  public setInstanceMap = (instances: IInstanceMap) => {
    this.setState({ instances });
  };
  public render() {
    if (!this.props.show) {
      return <div style={{ display: "none" }} />;
    }
    const { selectedInstance, selectedInstanceFrame, instances } = this.state;
    return (
      <div style={{ display: "none" }}>
        <Popout
          onClose={this.props.onClose}
          name="inspector"
          options={{
            menubar: false,
            toolbar: false,
            status: false,
            scrollbars: false
          }}
        >
          <link
            href="https://fonts.googleapis.com/css?family=Open+Sans&display=swap"
            rel="stylesheet"
          />
          <style>
            {`body, html {
                    margin: 0;
                padding: 0;
            }`}
          </style>
          <div
            style={{
              fontFamily: "'Open Sans', sans-serif",
              margin: 0,
              padding: 0
            }}
          >
            <Timeline
              instances={instances}
              selectedInstance={selectedInstance}
              selectedInstanceFrame={selectedInstanceFrame}
              selectInstanceFrame={this.selectInstanceFrame}
            />
            <Frames
              instances={instances}
              selectedInstance={selectedInstance}
              selectedInstanceFrame={selectedInstanceFrame}
            />
          </div>
        </Popout>
      </div>
    );
  }
}
export default Inspector;
