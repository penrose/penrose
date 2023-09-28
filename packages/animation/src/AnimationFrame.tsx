import { Simple } from "@penrose/components/dist/Simple";
import { toSVG } from "@penrose/core";
import React from "react";
import { compileTrio } from "./animation";
import { trio1 } from "./trio";

interface AnimationFrameState {
  isUpdating: boolean;
}

interface AnimationFrameProps {
  frameTitle: string;
}

export class AnimationFrame extends React.Component<
  AnimationFrameProps,
  AnimationFrameState
> {
  frameTitle: string;
  readonly canvasRef = React.createRef<HTMLDivElement>();
  constructor(props: AnimationFrameProps) {
    super(props);
    this.frameTitle = props.frameTitle;
    this.state = { isUpdating: false };
  }

  componentDidMount(): void {
    this.setState({ isUpdating: true });
    this.renderSVG();
  }

  renderSVG = async () => {
    if (this.canvasRef.current !== null) {
      const node = this.canvasRef.current;
      const converged = await compileTrio(trio1);
      const rendered = await toSVG(converged, async () => undefined, "one");
      if (node.firstChild !== null) {
        node.replaceChild(rendered, node.firstChild);
      } else {
        node.appendChild(rendered);
      }
      this.setState({ isUpdating: false });
    } else {
      console.log(this.canvasRef, "is null");
    }
  };

  render = () => {
    const simple = new Simple({
      domain: trio1.domain,
      substance: trio1.substance,
      style: trio1.style,
      variation: trio1.variation,
    });
    return (
      <div id={"frame"}>
        <div id={"svg-bbox"} ref={this.canvasRef} />
        {"hello!"}
        {simple.render()}
      </div>
    );
  };
}
