import React from "react";
import { SynthesizedSubstance } from "./Content";
import { Gridbox } from "./Gridbox";

export interface GridProps {
  style: any;
  domain: any;
  progs: SynthesizedSubstance[];
}

export class Grid extends React.Component<GridProps> {
  constructor(props: GridProps) {
    super(props);
  }

  updateGrid = () => {};

  render() {
    return (
      <div className="-m-2 flex flex-wrap">
        {this.props.progs.map((s) => (
          <Gridbox
            substance={s}
            domain={this.props.domain}
            style={this.props.style}
          />
        ))}
      </div>
    );
  }
}
