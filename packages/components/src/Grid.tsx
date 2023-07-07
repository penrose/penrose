import { PathResolver, PenroseState, isOptimized } from "@penrose/core";

import * as _ from "lodash";
import React from "react";
import styled from "styled-components";
import { Gridbox, GridboxProps } from "./Gridbox.js";

type DiagramSource = {
  style: string;
  domain: string;
  substance: string;
  variation: string;
};

export interface GridProps {
  diagrams: DiagramSource[];
  metadata: (i: number) => {
    name: string;
    data: string;
  }[];
  header: (i: number) => string;
  onSelected?: (n: number) => void;
  onComplete?: () => void;
  onStateUpdate: (n: number, s: PenroseState) => void;
  imageResolver?: PathResolver;
  gridBoxProps?: Partial<GridboxProps>;
  selected?: number[];
}

const GridContainer = styled.main`
  flex-grow: 1;
  margin-left: "4rem";
`;

const GridContent = styled.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`;

const Placeholder = styled.div``;

const PlaceholderText = styled.h3`
  font-family: "Roboto Mono";
  color: ${(props) => props.theme.primary};
`;

interface GridState {
  optimized: boolean[];
}

export class Grid extends React.Component<GridProps, GridState> {
  constructor(props: GridProps) {
    super(props);
    this.state = {
      optimized: Array(props.diagrams.length),
    };
  }

  innerContent() {
    return this.props.diagrams.map(
      ({ substance, domain, style, variation }, i) => (
        <Gridbox
          {...this.props.gridBoxProps}
          key={`grid-${i}`}
          name={`grid-${i}`}
          header={this.props.header(i)}
          metadata={this.props.metadata(i)}
          domain={domain}
          style={style}
          gridIndex={i}
          substance={substance}
          variation={variation}
          excludeWarnings={[]}
          onSelected={this.props.onSelected}
          onStateUpdate={(n, state) => {
            // record opt status
            this.setState((prev) => {
              const optStatuses = [...prev.optimized];
              optStatuses[n] = isOptimized(state);
              // report opt completion when all are done
              if (this.props.onComplete && _.every(optStatuses))
                this.props.onComplete();
              return { ...prev, optStatuses };
            });
            // callback
            this.props.onStateUpdate(n, state);
          }}
          imageResolver={this.props.imageResolver}
          selected={this.props.selected && this.props.selected.includes(i)}
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
