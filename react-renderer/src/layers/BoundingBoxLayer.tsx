import * as React from "react";
import {ILayerProps} from "../types";
import {toScreen} from "../Util";

class BoundingBoxLayer extends React.Component<ILayerProps> {
  public render() {
    const {shapes, canvasSize} = this.props;
    return (
      <g>
        {shapes.map(([name, shape]: [string, any], key: number) => {
          // TODO: add bb support to other shapes
          if (name === "Text") {
            const [x, y] = toScreen(
              [shape.x.contents, shape.y.contents],
              canvasSize
            );
            // const {w, h} = shape;
            const {finalW, finalH} = shape;
            return (
              <rect
                key={key}
                width={finalW.contents}
                height={finalH.contents}
                transform={`translate(${x - finalW.contents / 2},${y -
                finalH.contents / 2})`}
                fill={"none"}
                pointerEvents={"none"}
                stroke={"blue"}
              />
            );
          }
          return <g key={key}/>;
        })}
      </g>
    );
  }
}

export default BoundingBoxLayer;
