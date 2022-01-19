import React from "react";
import {
  compileTrio,
  PenroseState,
  prepareState,
  RenderInteractive,
  resample,
  showError,
  stepUntilConvergence,
} from "@penrose/core";
import fetchResolver from "./fetchPathResolver";
import seedrandom from "seedrandom";

export interface ISimpleProps {
  domainString: string;
  substanceString: string;
  styleString: string;
  variation: string;
  initState?: PenroseState;
}

interface ISimpleState {
  state?: PenroseState;
}

class Simple extends React.Component<ISimpleProps, ISimpleState> {
  public readonly canvasRef = React.createRef<HTMLDivElement>();
  constructor(props: ISimpleProps) {
    super(props);
    this.state = { state: undefined };
  }
  getInitState = async (
    dsl: string,
    sub: string,
    sty: string,
    variation: string
  ): Promise<PenroseState | undefined> => {
    const compilerResult = compileTrio(seedrandom(variation), dsl, sub, sty);
    if (compilerResult.isOk()) {
      const initState: PenroseState = await prepareState(compilerResult.value);
      const stepped = stepUntilConvergence(initState);
      if (stepped.isOk()) {
        return stepped.value;
      } else {
        console.log(showError(stepped.error));
      }
    } else {
      console.log(showError(compilerResult.error));
    }
  };
  componentDidMount = async () => {
    const {
      substanceString,
      styleString,
      domainString,
      variation,
    } = this.props;
    const state: PenroseState | undefined = await this.getInitState(
      domainString,
      substanceString,
      styleString,
      variation
    );
    this.setState({ state });
    this.renderCanvas(state);
  };

  renderCanvas = async (state: PenroseState | undefined) => {
    if (this.canvasRef.current === null) {
      return <div>rendering...</div>;
    } else {
      const node = this.canvasRef.current;
      if (state) {
        // if (!stateConverged(state)) {
        const newState = stepUntilConvergence(state).unsafelyUnwrap();
        this.setState({
          state: newState,
        });
        // }
        const renderedState: SVGSVGElement = await RenderInteractive(
          newState,
          this.updateState,
          fetchResolver
        );
        if (node.firstChild !== null) {
          node.replaceChild(renderedState, node.firstChild);
        } else {
          node.appendChild(renderedState);
        }
      } else {
        console.log("state is undefined");
      }
    }
  };

  updateState = (state: PenroseState): void => {
    this.setState({ state });
    this.renderCanvas(state);
  };

  resampleState = (): void => {
    const NUM_SAMPLES = 1;
    const { state: oldState } = this.state;
    if (oldState) {
      const resampled = resample(oldState, NUM_SAMPLES);
      const converged = stepUntilConvergence(resampled);
      const newState = converged.unsafelyUnwrap();
      this.setState({ state: newState });
      this.renderCanvas(newState);
    }
  };

  render = () => {
    return (
      <div style={{ width: "100%", height: "100%" }} ref={this.canvasRef} />
    );
  };
}

export { Simple };
