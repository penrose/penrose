import * as React from "react";
import IViewProps from "./IViewProps";
import styled from "styled-components";

const LogLine = styled.li`
  list-style-type: none;
  font-family: monospace;
  margin-left: 0;
  padding: 0.5em;
  width: 100%;
  border-bottom: 1px solid gray;
`;

class LogView extends React.Component<IViewProps> {
  public render() {
    // const { connectionLog } = this.props;
    return <ul style={{ padding: 0 }}></ul>;
  }
}

export default LogView;
