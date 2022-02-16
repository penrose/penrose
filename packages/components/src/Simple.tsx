import {
  compileTrio,
  PenroseState,
  prepareState,
  RenderInteractive,
  RenderStatic,
  resample,
  showError,
  stepUntilConvergence,
  variationSeeds,
} from "@penrose/core";
import React from "react";
import fetchResolver from "./fetchPathResolver";

export interface ISimpleProps {
  domain: string;
  substance: string;
  style: string;
  variation: string;
  interactive?: boolean; // considered true by default
}

class Simple extends React.Component<ISimpleProps> {
  readonly canvasRef = React.createRef<HTMLDivElement>();
  penroseState: PenroseState | undefined = undefined;

  compile = async (): Promise<void> => {
    this.penroseState = undefined;
    const compilerResult = compileTrio(this.props);
    if (compilerResult.isOk()) {
      // resample because initial sampling did not use the special sampling seed
      this.penroseState = resample(await prepareState(compilerResult.value));
    } else {
      console.log(showError(compilerResult.error));
    }
  };

  converge = async (): Promise<void> => {
    if (this.penroseState) {
      const stepped = stepUntilConvergence(this.penroseState);
      if (stepped.isOk()) {
        this.penroseState = stepped.value;
      } else {
        console.log(showError(stepped.error));
      }
    }
  };

  componentDidMount = async () => {
    await this.compile();
    await this.converge();
    this.renderCanvas();
  };

  componentDidUpdate = async (prevProps: ISimpleProps) => {
    if (
      this.props.domain !== prevProps.domain ||
      this.props.substance !== prevProps.substance ||
      this.props.style !== prevProps.style ||
      !this.penroseState
    ) {
      await this.compile();
      await this.converge();
      this.renderCanvas();
    } else if (this.props.variation !== prevProps.variation) {
      this.penroseState.seeds = variationSeeds(this.props.variation).seeds;
      this.penroseState = resample(this.penroseState);
      await this.converge();
      this.renderCanvas();
    } else if (this.props.interactive !== prevProps.interactive) {
      this.renderCanvas();
    }
  };

  renderCanvas = async () => {
    if (this.canvasRef.current === null) {
      return <div>rendering...</div>;
    } else {
      const node = this.canvasRef.current;
      if (this.penroseState) {
        const renderedState: SVGSVGElement = await (this.props.interactive ===
        false
          ? RenderStatic(this.penroseState, fetchResolver)
          : RenderInteractive(
              this.penroseState,
              (newState) => {
                this.penroseState = newState;
                this.converge();
                this.renderCanvas();
              },
              fetchResolver
            ));
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

  render = () => {
    return (
      <div style={{ width: "100%", height: "100%" }} ref={this.canvasRef} />
    );
  };
}

export { Simple };
