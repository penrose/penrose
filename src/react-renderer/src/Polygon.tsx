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

class Polygon extends React.Component<IGPIPropsDraggable> {
    public render() {
	const { shape } = this.props;
	const { canvasSize } = this.props;
	const { onClick } = this.props;

	const fillColor = toHex(shape.fillColor.contents);
	const fillAlpha = shape.fillColor.contents[3];
	const strokeColor = toHex(shape.strokeColor.contents);
	const strokeAlpha = shape.strokeColor.contents[3];
	const thickness = shape.strokeWidth.contents;

        const ptListString = toPointListString(
            shape.points.contents,
            canvasSize
        );

	const tf = shape.transformation.contents;
	console.log("transformation", tf);
	const transformList = [tf.xScale, tf.ySkew, tf.xSkew,
			       tf.yScale, tf.dx, tf.dy];
	const penroseTransform = "matrix(" + transformList.join(" ") + ")";

	// Do Penrose transform, then SVG
	const transformStr = [penroseToSVG(canvasSize), penroseTransform].join(" ");
	console.log("transformStr", transformStr);

	return (
                <polygon
                  points={ptListString}

		  fill={fillColor}
		  fillOpacity={fillAlpha}

		  stroke={strokeColor}
		  strokeOpacity={strokeAlpha}
		  strokeDasharray={ shape.strokeStyle.contents === "dashed" ? "7, 5" : "" }
		  strokeWidth={thickness}

		  transform={transformStr}

		  onMouseDown={onClick}
		>
		<title>{shape.name.contents}</title>
		</polygon>
	);
    }
}
export default draggable(Polygon);
