import * as React from "react";
import { staticMap } from "./componentMap";

interface IProps {
  substanceMetadata?: string;
  styleMetadata?: string;
  elementMetadata?: string;
  otherMetadata?: string;
  style?: any;
  penroseVersion?: string;
  state: any;
}

class StaticCanvas extends React.Component<IProps> {
  // public readonly svg = React.createRef<SVGSVGElement>();
  public readonly canvasSize: [number, number] = [800, 700];

  public renderEntity = ([name, shape]: [string, object], key: number) => {
    // const component = componentMap[name];
    // if (component === undefined) {
    //   console.log(`Could not render GPI ${name}.`);
    //   return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    // }
    // if (this.svg.current === null) {
    //   console.log("SVG ref is null");
    //   return <g key={key} />;
    // }
    // const ctm = this.svg.current.getScreenCTM();

    if (name === "Circle" || name === "Text") {
      const component = staticMap[name];
      const canvasSize = this.canvasSize;
      return React.createElement(component, {
        key,
        shape,
        canvasSize
        //   ctm
      });
    } else {
      return <g key={key} />;
    }
  };

  public render() {
    const {
      substanceMetadata,
      styleMetadata,
      elementMetadata,
      otherMetadata,
      state,
      penroseVersion
    } = this.props;
    const { shapesr } = state;

    // if (!shapesr) {
    //   return <svg ref={this.svg} />;
    // }

    return (
      <svg
        xmlns="http://www.w3.org/2000/svg"
        version="1.2"
        width="100%"
        height="100%"
        // ref={this.svg}
        viewBox={`0 0 ${this.canvasSize[0]} ${this.canvasSize[1]}`}
      >
        <desc>
          {`This diagram was created with Penrose (https://penrose.ink)${
            penroseVersion ? " version " + penroseVersion : ""
          } on ${new Date()
            .toISOString()
            .slice(
              0,
              10
            )}. If you have any suggestions on making this diagram more accessible, please contact us.\n`}
          {substanceMetadata && `${substanceMetadata}\n`}
          {styleMetadata && `${styleMetadata}\n`}
          {elementMetadata && `${elementMetadata}\n`}
          {otherMetadata && `${otherMetadata}`}
        </desc>
        {shapesr.map(this.renderEntity)}
      </svg>
    );
  }
}

export default StaticCanvas;
