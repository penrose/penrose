import * as React from "react";

import { IRendererEvents, EventHandler } from "../Protocol";
import Timeline from "./views/Timeline";
import { ConnectionStatus } from "src/module";
import viewMap from "./views/viewMap";
import { Tabs, TabList, Tab, TabPanels, TabPanel } from "@reach/tabs";
import "@reach/tabs/styles.css";
import ErrorBoundary from "./ErrorBoundary";

interface IProps {
  history: State[];
  onClose(): void;
}

export interface IInspectState {
  // connectionLog: Array<ConnectionStatus | string>;
  selectedFrame: number;
  selectedView: string;
}

class Inspector extends React.Component<IProps, IInspectState> {
  public readonly state = {
    // connectionLog: [],
    selectedFrame: -1,
    selectedView: "frames",
  };
  // public appendToConnectionLog = (status: ConnectionStatus | string) =>
  // this.setState({ connectionLog: [...this.state.connectionLog, status] });

  public selectFrame = (frame: number) => {
    this.setState({
      selectedFrame: frame === this.state.selectedFrame ? -1 : frame,
    });
  };
  public render() {
    const { selectedFrame } = this.state;
    const { history } = this.props;
    const currentFrame =
      history.length === 0
        ? null
        : selectedFrame === -1
        ? history[history.length - 1]
        : history[selectedFrame];
    const commonProps = {
      selectFrame: this.selectFrame,
      frame: currentFrame,
      frameIndex: selectedFrame,
      history,
    };
    // console.log(currentFrame);
    return (
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          height: "100%",
          overflow: "hidden",
          boxSizing: "border-box",
          paddingBottom: "1em",
        }}
      >
        <Timeline {...commonProps} />
        <div style={{ overflow: "hidden", flexGrow: 1, flexShrink: 1 }}>
          <Tabs>
            <TabList>
              {Object.keys(viewMap).map((view: string) => (
                <Tab key={`tab-${view}`}>{view}</Tab>
              ))}
            </TabList>
            <TabPanels>
              {Object.keys(viewMap).map((view: string) => (
                <TabPanel key={`panel-${view}`}>
                  <div
                    style={{
                      height: "100%",
                      overflow: "auto",
                      boxSizing: "border-box",
                    }}
                  >
                    <ErrorBoundary>
                      {React.createElement(viewMap[view], commonProps)}
                    </ErrorBoundary>
                  </div>
                </TabPanel>
              ))}
            </TabPanels>
          </Tabs>
        </div>
      </div>
    );
  }
}
export default Inspector;
