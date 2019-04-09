import * as React from "react";
import { toHex, toPointListString } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

const penroseToSVG = (canvasSize: [number, number]) => {
    const [width, height] = canvasSize;
    const flipYStr = "matrix(1 0 0 -1 0 0)";
    const translateStr = "translate(" + width / 2 + ", " + height / 2 + ")";
    // Flip Y direction, then translate shape to origin mid-canvas
    return [translateStr, flipYStr].join(" "); 
};

class CircleTransform extends React.Component<IGPIPropsDraggable> {
    public render() {
	const { shape } = this.props;
	const { canvasSize } = this.props;
	const { onClick } = this.props;

	// const [x, y] = [shape.x.contents, shape.y.contents];
	const fillColor = toHex(shape.color.contents);
	const fillAlpha = shape.color.contents[3];
	const strokeColor = toHex(shape.strokeColor.contents);
	const strokeAlpha = shape.strokeColor.contents[3];
	const thickness = shape.strokeWidth.contents;

	const tf = shape.transformation.contents;
	console.log("transformation", tf);
	const transformList = [tf.xScale, tf.ySkew, tf.xSkew,
			       tf.yScale, tf.dx, tf.dy];
	const penroseTransform = "matrix(" + transformList.join(" ") + ")";

	// Do Penrose transform, then SVG
	const transformStr = [penroseToSVG(canvasSize), penroseTransform].join(" ");
	console.log("transformStr", transformStr);

	// Note: the debug polygon already has the penrose transform applied so we only apply the toSVG transform
        const ptListString = toPointListString(
            shape.polygon.contents,
            canvasSize
        );

	return (
		<g>
		<circle
		  cx={-0.0}
		  cy={-0.0}
		  r={1.0}
		  fill={fillColor}
		  fillOpacity={fillAlpha}
		  stroke={strokeColor}
		  strokeOpacity={strokeAlpha}
		  strokeDasharray={ shape.strokeStyle.contents === "dashed" ? "7, 5" : "" }
		  strokeWidth={thickness}
		  onMouseDown={onClick}

            	  transform={transformStr}
		>
		  <title>{shape.name.contents}</title>
		</circle>

                <polygon
                  points={ptListString}
		  fillOpacity="0"
		  stroke="red"

	    transform={penroseToSVG(canvasSize)}

		  onMouseDown={onClick}
		/>
		</g>
	);
    }
}
export default draggable(CircleTransform);
