import { toSVG } from "@penrose/core";
import React, { RefObject } from "react";
import { compileTrio } from "./animation";
import { trio1, trio2 } from "./trio";

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
  readonly frameRef1 = React.createRef<HTMLDivElement>();
  readonly frameRef2 = React.createRef<HTMLDivElement>();
  constructor(props: AnimationFrameProps) {
    super(props);
    this.frameTitle = props.frameTitle;
    this.state = { isUpdating: false };
  }

  componentDidMount(): void {
    this.setState({ isUpdating: true });
    this.renderSVG(this.frameRef1, trio1);
    this.renderSVG(this.frameRef2, trio2);
  }

  renderSVG = async (ref: RefObject<HTMLDivElement>, trio: any) => {
    if (ref.current !== null) {
      const node = ref.current;
      const converged = await compileTrio(trio);
      const rendered = await toSVG(converged, async () => undefined, "one");
      if (node.firstChild !== null) {
        node.replaceChild(rendered, node.firstChild);
      } else {
        node.appendChild(rendered);
      }
      this.setState({ isUpdating: false });
    } else {
      console.log(ref.current, "is null");
    }
    // animation();  does nothing
  };

  render = () => {
    return (
      <div style={{ padding: "20px", fontFamily: "Arial" }}>
        <div>
          <h1>{"Approach: Baseline"}</h1>
        </div>
        <div
          style={{
            display: "flex",
            width: "100vw",
            overflow: "auto",
          }}
        >
          <div
            id={"frame1"}
            ref={this.frameRef1}
            style={{
              width: "400px",
              height: "400px",
              border: "2px solid gray",
            }}
          />
          <div
            id={"frame2"}
            ref={this.frameRef2}
            style={{
              width: "400px",
              height: "400px",
              border: "2px solid gray",
              borderLeft: "none",
            }}
          />
        </div>
      </div>
    );
  };
}
