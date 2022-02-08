import { PenroseError, PenroseState } from "@penrose/core";
import { Tab, TabList, TabPanel, TabPanels, Tabs } from "@reach/tabs";
import "@reach/tabs/styles.css";
import { ISettings } from "App";
import * as React from "react";
import ErrorBoundary from "./ErrorBoundary";
import IViewProps from "./views/IViewProps";
import viewMap from "./views/viewMap";

interface IProps {
  currentState: PenroseState | undefined;
  history: PenroseState[];
  error: PenroseError | undefined;
  onClose(): void;
  modCanvas(state: PenroseState): void;
  settings: ISettings;
  setSettings(ISettings): void;
  reset(): void;
}

export interface IInspectState {
  // connectionLog: Array<ConnectionStatus | string>;
  selectedFrame: number;
  selectedView: number;
}

class Inspector extends React.Component<IProps, IInspectState> {
  public readonly state = {
    // connectionLog: [],
    selectedFrame: -1,
    selectedView: 0,
  };
  // public appendToConnectionLog = (status: ConnectionStatus | string) =>
  // this.setState({ connectionLog: [...this.state.connectionLog, status] });

  public selectFrame = (frame: number): void => {
    this.setState({
      selectedFrame: frame === this.state.selectedFrame ? -1 : frame,
    });
    this.props.modCanvas(
      this.props.history[
        frame === this.state.selectedFrame
          ? this.props.history.length - 1
          : frame
      ]
    );
  };
  public render(): JSX.Element {
    const { selectedFrame, selectedView } = this.state;
    const {
      currentState,
      history,
      modCanvas,
      error,
      settings,
      setSettings,
      reset,
    } = this.props;
    const commonProps: IViewProps = {
      selectFrame: this.selectFrame,
      // frame: currentFrame,
      frame: currentState, // HACK: since history is disabled, we pass in the current state so the tab always shows the current state
      frameIndex: selectedFrame,
      history,
      modShapes: modCanvas,
      error,
      settings,
      setSettings,
      reset,
    };
    return (
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          height: "100%",
          overflow: "hidden",
          boxSizing: "border-box",
          marginBottom: "1em",
        }}
      >
        <div style={{ overflow: "hidden", flexGrow: 1, flexShrink: 1 }}>
          <Tabs
            index={selectedView}
            onChange={(idx: number) => this.setState({ selectedView: idx })}
          >
            <TabList>
              {Object.keys(viewMap).map((view: string) => (
                <Tab key={`tab-${view}`}>
                  {view}
                  {view === "errors" && error && (
                    <span style={{ fontWeight: 800, color: "#ff5c5c" }}>
                      {" "}
                      (1)
                    </span>
                  )}
                </Tab>
              ))}
            </TabList>
            <TabPanels>
              {Object.keys(viewMap).map((view: string, idx: number) => (
                <TabPanel key={`panel-${view}`}>
                  <div
                    style={{
                      height: "100%",
                      overflow: "auto",
                      boxSizing: "border-box",
                      paddingBottom: "100px",
                    }}
                  >
                    {idx === selectedView && (
                      <ErrorBoundary>
                        {React.createElement(viewMap[view], commonProps)}
                      </ErrorBoundary>
                    )}
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
