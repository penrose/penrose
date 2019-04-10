import * as React from "react";
import { toScreen, toPointListString, penroseToSVG } from "../Util";
import memoize from "fast-memoize";
import { ILayerProps } from "../types";

const toPointListStringOld = memoize(
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
      const polygonColor = "red";
      const shapesWithPolygons = ["CircleTransform", "RectangleTransform", "Polygon", "CurveTransform"];

    return (
      <g>
        {shapes.map(([name, shape]: [string, any], key: number) => {

	    // Curve
	    // TODO: show control points of curve
          if (name === "Curve") {
            const ptListString = toPointListStringOld(
              shape.polyline.contents,
              canvasSize
            );
            return (
              <g key={key}>
                <polyline
                  points={ptListString}
                  stroke={polygonColor}
                  fillOpacity="0"
                />
              </g>
            );
          }

	    // Note: the debug polygon already has the Penrose transform applied so we only apply the transform to SVG space
	    // TODO: refactor this when all shapes have "polygon" property
	    else if (shapesWithPolygons.includes(name)) {
	       const ptListString = toPointListString(
		   shape.polygon.contents,
		   canvasSize
	       );

		return (
              <g key={key}>
                <polyline
                  points={ptListString}
		  fillOpacity="0"
		  stroke={polygonColor}
		  transform={penroseToSVG(canvasSize)}
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
