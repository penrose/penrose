import * as React from "react";
import { toHex, toPointListString, svgTransformString } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Polygon extends React.Component<IGPIPropsDraggable> {
    public render() {
	const { shape } = this.props;
	const { canvasSize } = this.props;
	const { onClick } = this.props;

	const fillColor = toHex(shape.fillColor.contents);
	const fillAlpha = shape.fillColor.contents[3];
	const strokeColor = toHex(shape.strokeColor.contents);
	const strokeAlpha = shape.strokeColor.contents.contents[3];
	const thickness = shape.strokeWidth.contents;

        const ptListString = toPointListString(
            shape.points.contents,
            canvasSize
        );

	const transformStr = svgTransformString(shape.transformation.contents, canvasSize);

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
