import * as React from "react";

import { IRendererEvents, IInstanceMap, EventHandler } from "../Protocol";
import Timeline from "./views/Timeline";
import { ConnectionStatus } from "src/module";
import viewMap from "./views/viewMap";
import { Tabs, TabList, Tab, TabPanels, TabPanel } from "@reach/tabs";
import "@reach/tabs/styles.css";

interface IProps {
  onReady(handler: EventHandler): void;
  onClose(): void;
}

export interface IInspectState {
  instances: IInstanceMap;
  connectionLog: Array<ConnectionStatus | string>;
  selectedInstance: string;
  selectedInstanceFrame: number;
  selectedView: string;
}

class Inspector extends React.Component<IProps, IInspectState> {
  public readonly state = {
    instances: {},
    connectionLog: [],
    selectedInstance: "",
    selectedInstanceFrame: -1,
    selectedView: "frames"
  };
  public appendToConnectionLog = (status: ConnectionStatus | string) =>
    this.setState({ connectionLog: [...this.state.connectionLog, status] });
  public readonly Events: IRendererEvents = {
    kind: "renderer",
    onConnectionStatus: this.appendToConnectionLog,
    onVersion: this.appendToConnectionLog,
    onCanvasState: (canvasState: any, id: string) => {
      const { selectedInstance, instances } = this.state;
      let instance = instances[id] || [];
      instance = [...instance, canvasState];
      this.setState({
        selectedInstance: selectedInstance === "" ? id : selectedInstance,
        instances: { ...instances, [id]: instance }
      });
    },
    onError: this.appendToConnectionLog
  };

  public componentDidMount() {
    this.props.onReady(this.Events);
  }
  public selectInstanceFrame = (frame: number) => {
    this.setState({
      selectedInstanceFrame:
        frame === this.state.selectedInstanceFrame ? -1 : frame
    });
  };
  public render() {
    return (
      <div>
        <Timeline
          selectInstanceFrame={this.selectInstanceFrame}
          {...this.state}
        />
        <Tabs>
          <TabList>
            {Object.keys(viewMap).map((view: string) => (
              <Tab key={`tab-${view}`}>{view}</Tab>
            ))}
          </TabList>
          <TabPanels>
            {Object.keys(viewMap).map((view: string) => (
              <TabPanel key={`panel-${view}`}>
                {React.createElement(viewMap[view], {
                  ...this.state,
                  selectInstanceFrame: this.selectInstanceFrame
                })}
              </TabPanel>
            ))}
          </TabPanels>
        </Tabs>
      </div>
    );
  }
}
export default Inspector;
