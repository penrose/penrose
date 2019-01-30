import * as React from "react";
import {toScreen} from "../Util";
import memoize from "fast-memoize";
import {ILayerProps} from "../types";

const toPointListString = memoize(
  (ptList: any[], canvasSize: [number, number]) =>
    ptList
      .map((coords: [number, number]) => {
        const pt = toScreen(coords, canvasSize);
        return pt[0].toString() + " " + pt[1].toString();
      })
      .join(" ")
);

class PolygonLayer extends React.Component<ILayerProps> {
  public render() {
    const {shapes, canvasSize} = this.props;
    // TODO: control points
    return (
      <g>
        {shapes.map(([name, shape]: [string, any], key: number) => {
          if (name === "Curve") {
            const ptListString = toPointListString(
              shape.polyline.contents,
              canvasSize
            );
            return (
              <g key={key}>
                <polyline
                  points={ptListString}
                  stroke="black"
                  fillOpacity="0"
                />
              </g>
            );
          }
          return <g key={key}/>;
        })}
      </g>
    );
  }
}

export default PolygonLayer;
