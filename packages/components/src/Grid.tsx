import { PathResolver, PenroseState } from "@penrose/core";

import React from "react";
import styled from "styled-components";
import { Gridbox } from "./Gridbox";

type DiagramSource = {
  style: string;
  domain: string;
  substance: string;
  variation: string;
};

export interface GridProps {
  diagrams: DiagramSource[];
  metadata: (
    i: number
  ) => {
    name: string;
    data: string;
  }[];
  onSelected?: (n: number) => void;
  onStateUpdate: (n: number, s: PenroseState) => void;
  imageResolver?: PathResolver;
}

const GridContainer = styled.main`
  flex-grow: 1;
  margin-left: "4rem";
`;

const GridContent = styled.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
`;

const Placeholder = styled.div``;

const PlaceholderText = styled.text``;

// const PlaceholderText = styled(Typography)(({ theme }) => ({
//   color: theme.palette.primary.main,
//   fontFamily: "Roboto Mono",
// }));

export class Grid extends React.Component<GridProps> {
  constructor(props: GridProps) {
    super(props);
  }

  setSrcState = (newState: PenroseState) => {
    this.setState({
      srcState: newState,
    });
  };

  innerContent() {
    return this.props.diagrams.map(
      ({ substance, domain, style, variation }, i) => (
        <Gridbox
          key={`grid-${i}`}
          header={`Diagram ${i}`}
          metadata={this.props.metadata(i)}
          domain={domain}
          style={style}
          gridIndex={i}
          substance={substance}
          variation={variation}
          onSelected={this.props.onSelected}
          onStateUpdate={this.props.onStateUpdate}
          imageResolver={this.props.imageResolver}
        />
      )
    );
  }

  render() {
    const content =
      this.props.diagrams.length === 0 ? (
        <Placeholder>
          <PlaceholderText>
            {"(Generated diagrams will appear here)"}
          </PlaceholderText>
        </Placeholder>
      ) : (
        this.innerContent()
      );
    return (
      <GridContainer>
        <GridContent>{content}</GridContent>
      </GridContainer>
    );
  }
}
