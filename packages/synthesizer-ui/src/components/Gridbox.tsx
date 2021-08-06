import {
  compileTrio,
  evalEnergy,
  prepareState,
  RenderStatic,
  prettySubstance,
  stepUntilConvergence,
  resample,
} from "@penrose/core";
import { SynthesizedSubstance } from "./Content";
import styled from "styled-components";
import React from "react";

export interface GridboxProps {
  domain: string;
  style: string;
  substance: SynthesizedSubstance;
  progNumber: number;
  srcProg: any;
  onStaged: (n: number, s: string) => void;
}

const Section = styled.section`
  margin: 0.5rem;
  width: 25rem;
  height: 25rem;
  border: 1px solid gray;
`;

const LowEnergy = styled(Section)`
  border: 3px solid green;
`;

const HighEnergy = styled(Section)`
  border: 3px solid red;
`;

const Header = styled.section`
  width: calc(100% - 0.5rem);
  height: 1.75rem;
  border-bottom: 1px solid black;
  font-size: 1rem;
  color: gray;
  padding: 0.25rem;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
`;

const Body = styled.section`
  font-family: "Courier New", sans-serif;
  height: calc(25rem - 4.25rem);
  font-size: 0.8rem;
  color: black;
  overflow: auto;
  padding: 1rem;
  white-space: pre-wrap;
`;

const programString = (stmts: string, ops: string) => {
  return `${stmts}
-----
Operations:
${ops}
`;
};

interface GridboxState {
  showDiagram: boolean;
  isSelected: boolean;
  diagramSVG: string;
}

export class Gridbox extends React.Component<GridboxProps, GridboxState> {
  private energy: number | undefined;
  constructor(props: GridboxProps) {
    super(props);
    this.state = {
      showDiagram: true,
      isSelected: false,
      diagramSVG: "",
    };
    this.energy = 0; // TODO
  }

  computeEnergy = async () => {
    let srcState: { val: any } = { val: {} };
    let optimizedState: { val: any } = { val: {} };
    const getState = async (prog: any, state: { val: any }) => {
      const res = compileTrio(
        this.props.domain,
        prettySubstance(prog),
        this.props.style
      );
      if (res.isOk()) {
        const st = await prepareState(res.value);
        state.val = st;
        return state;
      }
    };
    this.energy = undefined;
    getState(this.props.srcProg, srcState);
    getState(this.props.substance.prog, optimizedState);
    if (srcState.val) {
      const crossState = {
        ...optimizedState.val,
        constrFns: srcState.val.constrFns,
        objFns: srcState.val.objFns,
      };
      try {
        this.energy = evalEnergy(await prepareState(crossState));
      } catch (e) {
        console.log(e);
      }
    }
  };

  // TODO: this should really be put in a web worker, it blocks browser interaction
  async update() {
    const res = compileTrio(
      this.props.domain,
      prettySubstance(this.props.substance.prog),
      this.props.style
    );
    if (res.isOk()) {
      try {
        // https://stackoverflow.com/a/19626821
        // setTimeout causes this function to be pushed to bottom of call stack. Since Gridbox
        // component is rendered in an array, we want to delay ALL componentDidMount calls until
        // after ALL gridboxes have been initially rendered.
        await new Promise((r) => setTimeout(r, 1));
        let state = await prepareState(res.value);
        state = resample(state, 1);
        const opt = stepUntilConvergence(state);
        if (opt.isErr()) {
          throw Error("optimization failed");
        }
        const optimized = opt.value;
        this.setState({ diagramSVG: RenderStatic(optimized).outerHTML });
      } catch (e) {
        console.log(e);
      }
    } else {
      throw res.error;
    }
  }

  async componentDidMount() {
    await this.update();
  }

  async componentDidUpdate(prevProps: GridboxProps) {
    if (this.props.substance.prog !== prevProps.substance.prog) {
      await this.update();
    }
  }

  toggleView = () => {
    this.setState({ showDiagram: !this.state.showDiagram });
  };

  checkboxClick = () => {
    this.setState({ isSelected: !this.state.isSelected });
    this.props.onStaged(this.props.progNumber, this.state.diagramSVG);
  };

  render() {
    const stmts = prettySubstance(this.props.substance.prog);
    return (
      <Section>
        <Header>
          <div>
            {this.props.progNumber === 0
              ? "Original Diagram"
              : `Mutated Program #${this.props.progNumber}`}
          </div>
          <input
            name="isStaged"
            type="checkbox"
            checked={this.state.isSelected}
            onChange={this.checkboxClick}
          />
        </Header>
        <div onClick={this.toggleView}>
          {this.state.showDiagram ? (
            <div
              style={{ width: "100%", height: "100%" }}
              dangerouslySetInnerHTML={{
                __html: this.state.diagramSVG,
              }}
            />
          ) : (
            <Body>
              {programString(
                stmts,
                this.props.progNumber === 0 ? "" : this.props.substance.ops
              )}
            </Body>
          )}
        </div>
      </Section>
    );
  }
}
