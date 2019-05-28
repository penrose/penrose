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

    return (
      <g>
        {shapes.map(([name, shape]: [string, any], key: number) => {

	    // TODO: show control points of bezier curve
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
	    // TODO: so far we only draw the first positive shape in the polygon, not other pieces or holes
	    else if (shape.hasOwnProperty("polygon")) {
		const positiveShape0 = shape.polygon.contents[0][0];

	       const ptListString = toPointListString(
		   positiveShape0,
		   canvasSize
	       );

		return (
              <g key={key}>
                <polygon
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
