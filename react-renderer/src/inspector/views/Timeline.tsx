import * as React from "react";

import styled from "styled-components";
import Canvas from "src/Canvas";
import IViewProps from "./IViewProps";

const TimelineStyled = styled.ul`
  background-color: rgba(0, 0, 0, 0.05);
  overflow-x: auto;
  overflow-y: hidden;
  display: flex;
  align-items: center;
  flex-wrap: nowrap;
  box-sizing: border-box;
  padding-top: 1em;
  padding-bottom: 1em;
  margin: 0;
  padding-left: 0.5em;
  padding-right: 1em;
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
  border-radius: 5px;
  overflow: hidden;
  box-shadow: rgba(0, 0, 0, 0.2) 0px 2px 3px 0px;
`;

class Timeline extends React.Component<IViewProps> {
  public timelineRef = React.createRef<any>();
  public componentDidUpdate = ({ instances, selectedInstance }: IViewProps) => {
    if (
      instances[selectedInstance] &&
      this.props.instances[selectedInstance] &&
      instances[selectedInstance].length !==
        this.props.instances[selectedInstance].length
    ) {
      this.timelineRef.current.scrollLeft = this.timelineRef.current.scrollWidth;
    }
  };
  public render() {
    const { instances, selectedInstance, selectedInstanceFrame } = this.props;
    return (
      <TimelineStyled ref={this.timelineRef}>
        {instances[selectedInstance]
          ? instances[selectedInstance].map((instance: any, k: number) => {
              return (
                <TimelineItem
                  selected={k === selectedInstanceFrame}
                  key={k}
                  onClick={() => this.props.selectInstanceFrame(k)}
                >
                  <Canvas
                    data={instance}
                    layers={[]}
                    lock={true}
                    updateData={() => void 0}
                  />
                </TimelineItem>
              );
            })
          : "none"}
      </TimelineStyled>
    );
  }
}

export default Timeline;
