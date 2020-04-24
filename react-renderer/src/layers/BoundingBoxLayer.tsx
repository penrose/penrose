import * as React from "react";
import { ILayerProps } from "../types";
import { toScreen } from "../Util";

class BoundingBoxLayer extends React.Component<ILayerProps> {
  public render() {
    const { shapes, canvasSize } = this.props;
    return (
      <g>
        {shapes.map(({ shapeType, properties }: Shape, key: number) => {
          // TODO: add bb support to other shapes
          if (shapeType === "Text") {
            const [x, y] = toScreen(
              [
                properties.x.contents as number,
                properties.y.contents as number,
              ],
              canvasSize
            );
            const w = properties.w.contents as number;
            const h = properties.h.contents as number;
            return (
              <rect
                key={key}
                width={w}
                height={h}
                transform={`translate(${x - w / 2},${y - h / 2})`}
                fill={"none"}
                pointerEvents={"none"}
                stroke={"blue"}
              />
            );
          }
          return <g key={key} />;
        })}
      </g>
    );
  }
}

export default BoundingBoxLayer;
