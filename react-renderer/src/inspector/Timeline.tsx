import * as React from "react";

import styled from "styled-components";
import Canvas from "src/Canvas";
import { IInstanceMap } from "src/Protocol";

interface IProps {
  instances: IInstanceMap;
  selectedInstance: string;
  selectedInstanceFrame: number;

  selectInstanceFrame(frame: number): void;
}

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

// const TimelineItem = styled.li`
//   display: inline-block;
//   /* cursor: pointer; */
//   /* width: 75px; */
//   /* height: 65px; */
/* border: ${({ selected }: any) => (selected ? "3px" : "1px")} solid */
/* ${({ selected }: any) => (selected ? "#40B4F7" : "gray")}; */
//   /* margin-left: 0.5em; */
//   /* border-radius: 5px; */
//   /* overflow: hidden; */
//   /* box-shadow: rgba(0, 0, 0, 0.2) 0px 2px 3px 0px; */
// `;

const TimelineItemStyle = (selected: boolean) => ({
  display: "inline-block",
  cursor: "pointer",
  width: "30px",
  height: "26px",
  marginLeft: "0.5em",
  border: `${selected ? "3px" : "1px"} solid ${selected ? "#40B4F7" : "gray"}`,
  borderRadius: "5px",
  overflow: "hidden",
  flexShrink: 0,
  boxShadow: "rgba(0,0,0,0.2) 0px 2px 3px 0px"
});

class Timeline extends React.Component<IProps> {
  public timelineRef = React.createRef<any>();
  public componentDidUpdate = ({ instances, selectedInstance }: IProps) => {
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
                <li
                  key={k}
                  style={TimelineItemStyle(k === selectedInstanceFrame)}
                  onClick={() => this.props.selectInstanceFrame(k)}
                >
                  <Canvas
                    data={instance}
                    layers={[]}
                    lock={true}
                    updateData={() => void 0}
                  />
                </li>
              );
            })
          : "none"}
      </TimelineStyled>
    );
  }
}

export default Timeline;
