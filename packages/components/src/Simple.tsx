import {
  compileTrio,
  PathResolver,
  PenroseError,
  PenroseState,
  prepareState,
  RenderInteractive,
  RenderStatic,
  resample,
  showError,
  stateConverged,
  stepState,
  stepUntilConvergence,
} from "@penrose/core";
import React from "react";
import fetchResolver from "./fetchPathResolver";

export interface SimpleProps {
  domain: string;
  substance: string;
  style: string;
  variation: string;
  stepSize?: number;
  interactive?: boolean; // considered true by default
  animate?: boolean; // considered false by default
  onFrame?: (frame: PenroseState) => void;
  imageResolver?: PathResolver;
  name?: string;
}

export interface SimpleState {
  error?: PenroseError;
}

class Simple extends React.Component<SimpleProps, SimpleState> {
  readonly canvasRef = React.createRef<HTMLDivElement>();
  penroseState: PenroseState | undefined = undefined;
  timerID: number | undefined = undefined; // for animation

  constructor(props: SimpleProps) {
    super(props);
    this.state = {
      error: undefined,
    };
  }

  compile = async (): Promise<void> => {
    this.penroseState = undefined;
    this.setState({ error: undefined });
    const compilerResult = await compileTrio(this.props);
    if (compilerResult.isOk()) {
      this.penroseState = await prepareState(compilerResult.value);
      this.setState({ error: undefined }); // clear out errors
    } else {
      this.setState({ error: compilerResult.error });
    }
  };

  converge = async (): Promise<void> => {
    if (this.penroseState) {
      const stepped = stepUntilConvergence(this.penroseState);
      if (stepped.isOk()) {
        this.penroseState = stepped.value;
      } else {
        this.setState({ error: stepped.error });
      }
    }
  };

  tick = () => {
    if (
      this.props.animate &&
      this.penroseState &&
      !stateConverged(this.penroseState)
    ) {
      this.penroseState = stepState(
        this.penroseState,
        this.props.stepSize ?? 1
      );
      this.renderCanvas();
    }
  };

  componentDidMount = async () => {
    await this.compile();
    if (!this.props.animate) {
      await this.converge();
    }
    this.renderCanvas();
    this.timerID = window.setInterval(() => this.tick(), 1000 / 60);
  };

  componentDidUpdate = async (prevProps: SimpleProps) => {
    // re-compile if the programs change
    if (
      this.props.domain !== prevProps.domain ||
      this.props.substance !== prevProps.substance ||
      this.props.style !== prevProps.style
    ) {
      await this.compile();
      if (!this.props.animate) {
        await this.converge();
      }
      this.renderCanvas();
      return;
    }

    // update the component only if there's no error
    // in the case of an error, they component should not attempt to re-render
    if (this.penroseState && !this.state.error) {
      if (
        this.props.variation !== prevProps.variation ||
        this.props.animate !== prevProps.animate
      ) {
        this.penroseState.variation = this.props.variation;
        this.penroseState = resample(this.penroseState);
        if (!this.props.animate) {
          await this.converge();
        }
        this.renderCanvas();
        return;
      } else if (this.props.interactive !== prevProps.interactive) {
        this.renderCanvas();
        return;
      }
    }
  };

  componentWillUnmount = () => {
    clearInterval(this.timerID);
  };

  renderCanvas = async () => {
    if (this.canvasRef.current === null) {
      return <div>rendering...</div>;
    } else {
      const node = this.canvasRef.current;
      if (this.penroseState) {
        const renderedState: SVGSVGElement = await (this.props.interactive ===
        false
          ? RenderStatic(
              this.penroseState,
              this.props.imageResolver ?? fetchResolver,
              this.props.name ?? ""
            )
          : RenderInteractive(
              this.penroseState,
              async (newState: PenroseState) => {
                this.penroseState = newState;
                if (!this.props.animate) {
                  await this.converge();
                }
                this.renderCanvas();
              },
              this.props.imageResolver ?? fetchResolver,
              this.props.name ?? ""
            ));
        // to avoid overflowing the parent div, force the height and width to be 100%
        renderedState.setAttribute("height", "100%");
        renderedState.setAttribute("width", "100%");
        if (node.firstChild !== null) {
          node.replaceChild(renderedState, node.firstChild);
        } else {
          node.appendChild(renderedState);
        }
        // propagate state update
        if (this.props.onFrame) {
          this.props.onFrame(this.penroseState);
        }
      } else {
        return <div>rendering...</div>;
      }
    }
  };

  render = () => {
    const { error } = this.state;
    return (
      <div style={{ width: "100%", height: "100%" }}>
        {!error && (
          <div style={{ width: "100%", height: "100%" }} ref={this.canvasRef} />
        )}
        {error && (
          <div style={{ padding: "1em", height: "100%" }}>
            <div style={{ fontWeight: 700 }}>1 error:</div>
            <div style={{ fontFamily: "monospace" }}>
              {showError(error)
                .toString()
                .split("\n")
                .map((line: string, key: number) => (
                  <p key={`err-ln-${key}`} style={{ margin: 0 }}>
                    {line}
                  </p>
                ))}
            </div>
          </div>
        )}
      </div>
    );
  };
}

export { Simple };
