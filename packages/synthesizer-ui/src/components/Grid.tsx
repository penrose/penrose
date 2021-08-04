import React from "react";
import { SynthesizedSubstance } from "./Content";
import { Gridbox } from "./Gridbox";
import styled from "styled-components";
import { prettySubstance } from "@penrose/core";

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
  height: 100%;
  width: 100%;
  margin-left: 1rem;
`;

export class Grid extends React.Component<GridProps> {
  constructor(props: GridProps) {
    super(props);
  }

  updateGrid = () => {};

  render() {
    console.log("making grid", this.props.progs);
    const boxes = this.props.progs.map((s, i) => (
      <Gridbox
        substance={prettySubstance(s.prog)}
        key={`grid-${i}`}
        domain={this.props.domain}
        style={this.props.style}
      />
    ));
    return <Section>{boxes}</Section>;
  }
}
