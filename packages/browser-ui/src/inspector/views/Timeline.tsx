import { PenroseState, RenderStatic } from "@penrose/core";
import * as React from "react";

import styled from "styled-components";
import IViewProps from "./IViewProps";

const TimelineStyled = styled.ul`
  background-color: rgba(0, 0, 0, 0.05);
  overflow-x: auto;
  overflow-y: hidden;
  display: flex;
  flex-shrink: 0;
  align-items: center;
  flex-wrap: nowrap;
  box-sizing: border-box;
  margin: 0;
  padding-left: 0.5em;
  padding-right: 2em;
  width: 100%;
  list-style-type: none;
`;

const TimelineItem = styled.li<any>`
  display: inline-block;
  cursor: pointer;
  width: 30px;
  height: 26px;
  flex-shrink: 0;
  border: ${({ selected }: any) => (selected ? "3px" : "1px")} solid
    ${({ selected }: any) => (selected ? "#40B4F7" : "gray")};
  margin-left: 0.5em;
  margin-top: 5px;
  margin-bottom: 5px;
  border-radius: 5px;
  overflow: hidden;
  box-shadow: rgba(0, 0, 0, 0.2) 0px 2px 3px 0px;
`;

class Timeline extends React.Component<IViewProps> {
  public timelineRef = React.createRef<any>();
  public componentDidUpdate = ({ history }: IViewProps) => {
    if (history.length !== this.props.history.length) {
      this.timelineRef.current.scrollLeft = this.timelineRef.current.scrollWidth;
    }
  };
  public render() {
    const { frameIndex, history } = this.props;
    return (
      <TimelineStyled ref={this.timelineRef}>
        {history.map((frame: PenroseState, k: number) => {
          return (
            <TimelineItem
              selected={k === frameIndex}
              key={k}
              onClick={() => this.props.selectFrame(k)}
              dangerouslySetInnerHTML={{
                __html: RenderStatic(frame).outerHTML,
              }}
            />
          );
        })}
      </TimelineStyled>
    );
  }
}

export default Timeline;
