import React from "react";
import { SynthesizedSubstance } from "./Content";
import { Gridbox } from "./Gridbox";
import styled from "styled-components";
import {
  compileTrio,
  prepareState,
  prettySubstance,
  RenderStatic,
  stepUntilConvergence,
} from "@penrose/core";
import {
  Async,
  PromiseFn,
  useAsync,
  IfPending,
  IfFulfilled,
  IfRejected,
} from "react-async";

export interface GridProps {
  style: any;
  domain: any;
  progs: SynthesizedSubstance[];
}

const Section = styled.section`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: flex-start;
  height: calc(100vh - 6.75rem);
  border-left: 1px solid black;
  width: 100%;
  padding: 1rem;
  overflow: auto;
`;

const Placeholder = styled.section`
  border: 2px dashed gray;
  display: flex;
  justify-content: center;
  align-items: center;
  color: gray;
  width: 98%;
  font-size: 2rem;
  border-radius: 1rem;
`;

interface GridState {
  svgs: string[];
}

export class Grid extends React.Component<GridProps, GridState> {
  private svgs: string[];
  constructor(props: GridProps) {
    super(props);
    this.state = { svgs: [] };
    this.svgs = [];
  }

  innerContent() {
    console.log(this.props.progs);
    return this.props.progs.map((s, i) => (
      <Gridbox
        key={`grid-${i}`}
        domain={this.props.domain}
        style={this.props.style}
        progNumber={i}
        substance={s}
        srcProg={this.props.progs[0].prog}
      />
    ));
  }

  render() {
    console.log("making grid", this.props.progs);
    if (this.props.progs.length === 0) {
      return (
        <Section>
          <Placeholder>{"Generate diagrams to view them here"}</Placeholder>
        </Section>
      );
    } else {
      return <Section>{this.innerContent()}</Section>;
    }
  }
}
