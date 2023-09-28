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
  constructor(props: AnimationFrameProps) {
    super(props);
    this.frameTitle = props.frameTitle;
    this.state = { isUpdating: false };
  }

  renderSVG = async () => {
    const converged = await compileTrio(trio1);
    const rendered = await toSVG(converged, async () => undefined, "");
    const ele = document.getElementById("frame");
    ele?.appendChild(rendered);
    return rendered;
  };

  render = () => {
    const simple = new Simple({
      domain: trio1.setDomain,
      substance: trio1.set1substance,
      style: trio1.setStyle,
      variation: trio1.variation,
    });
    return (
      <div id={"frame"}>
        {"hello!"}
        {simple.render()}
      </div>
    );
  };
}
