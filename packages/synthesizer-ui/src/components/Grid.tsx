import React from "react";
import { SynthesizedSubstance } from "./Content";
import { Gridbox } from "./Gridbox";
import styled from "styled-components";

export interface GridProps {
  style: any;
  domain: any;
  progs: SynthesizedSubstance[];
}

const Section = styled.section`
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  margin-right: 2rem;
  background-color: green;
  height: auto;
`;

export class Grid extends React.Component<GridProps> {
  constructor(props: GridProps) {
    super(props);
  }

  updateGrid = () => {};

  render() {
    console.log("making grid", this.props.progs);
    return (
      <Section>
        {this.props.progs.map((s) => (
          <Gridbox
            substance={s}
            domain={this.props.domain}
            style={this.props.style}
          />
        ))}
      </Section>
    );
  }
}
