import {
  compileTrio,
  prepareState,
  RenderStatic,
  stepUntilConvergence,
} from "@penrose/core";
import { SynthesizedSubstance } from "./Content";
import styled from "styled-components";
import React from "react";

export interface GridboxProps {
  substance: string;
  domain: string;
  style: string;
}

const Section = styled.section`
  margin: 0.5rem;
  width: 10rem;
  height: 10rem;
  background: blue;
`;

interface GridboxState {
  isWaiting: boolean;
  diagramSVG: string;
}

export class Gridbox extends React.Component<GridboxProps, GridboxState> {
  // private diagramSVG: string;
  constructor(props: GridboxProps) {
    super(props);
    this.state = { isWaiting: false, diagramSVG: "" };
  }

  async componentDidMount() {
    this.setState({ isWaiting: true });
    console.log(this.props.substance);
    const res = compileTrio(
      this.props.domain,
      this.props.substance,
      this.props.style
    );
    console.log(res);
    if (res.isOk()) {
      console.log("result is ok!");
      try {
        const state = await prepareState(res.value);
        const opt = stepUntilConvergence(state);
        if (opt.isErr()) {
          throw Error("optimization failed");
        }
        const optimized = opt.value;
        this.setState({ diagramSVG: RenderStatic(optimized).outerHTML });
        console.log("svg!", this.state.diagramSVG);
      } catch (e) {
        this.setState({ isWaiting: false });
        throw e;
      }
    } else {
      throw res.error;
    }
    this.setState({ isWaiting: false });
  }

  render() {
    return <Section>{this.state ? this.state.diagramSVG : ""}</Section>;
  }
}
