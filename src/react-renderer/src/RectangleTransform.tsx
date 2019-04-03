import * as React from "react";
import { toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

const penroseToSVG = (canvasSize: [number, number]) => {
    const [width, height] = canvasSize;
    const flipYStr = "matrix(1 0 0 -1 0 0)";
    const translateStr = "translate(" + width / 2 + ", " + height / 2 + ")";
    // Flip Y direction, then translate shape to origin mid-canvas
    return [translateStr, flipYStr].join(" "); 
};

class RectangleTransform extends React.Component<IGPIPropsDraggable> {
    public render() {
	const { shape } = this.props;
	const { canvasSize } = this.props;
	const { onClick } = this.props;

	const [x, y] = [shape.x.contents, shape.y.contents];
	const fillColor = toHex(shape.color.contents);
	const fillAlpha = shape.color.contents[3];
	const strokeColor = toHex(shape.strokeColor.contents);
	const strokeAlpha = shape.strokeColor.contents[3];
	const thickness = shape.strokeWidth.contents;

	// https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
	// TODO: check that the transform respects screen space, SVG directionality, and the Style writer's intention
	// Right now increasing the y will move downward. Does that affect the skew factors?

	const tf = shape.transform.contents;
	console.log("transform", tf);
	const transformList = [tf.x_scale, tf.yx_fac, tf.xy_fac,
			       tf.y_scale, tf.dx, tf.dy];
	const penroseTransform = "matrix(" + transformList.join(" ") + ")";

	// Do Penrose transform, then SVG
	const transformStr = [penroseToSVG(canvasSize), penroseTransform].join(" ");
	console.log("transformStr", transformStr);

	return (
		<rect
            x={x - shape.sizeX.contents / 2}
            y={y - shape.sizeY.contents / 2}
            width={shape.sizeX.contents}
            height={shape.sizeY.contents}
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
		</rect>
	);
    }
}
export default draggable(RectangleTransform);
