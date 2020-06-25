import * as React from "react";
import IViewProps from "./IViewProps";
import { staticMap } from "src/componentMap";
import styled from "styled-components";
import { ObjectInspector } from "react-inspector";

const ShapeItem = styled.li<any>`
  display: block;
  padding: 1em;
  margin-top: -1px;
  border: 1px solid #d1d1d1;
  background-color: ${({ selected }: any) =>
    selected ? "#F9F9F9" : "#f0f0f0"};
  color: rgba(0, 0, 0, 0.5);
  font-family: monospace;
  display: flex;
  flex-direction: row;
  align-items: center;
  cursor: pointer;
`;

interface IState {
  selectedShape: number;
}

class ShapeView extends React.Component<IViewProps, IState> {
  public readonly state = { selectedShape: -1 };
  public setSelectedShape = (key: number) => {
    this.setState({ selectedShape: key });
  };
  public render() {
    const { frame } = this.props;
    if (frame === null) {
      return <div />;
    }

    const { selectedShape } = this.state;
    return (
      <div
        style={{
          display: "flex",
          width: "100%",
          height: "100%",
          overflow: "hidden",
        }}
      >
        <div
          style={{ overflowY: "auto", height: "100%", }}
        >
          <ul
            style={{
              listStyleType: "none",
              padding: "0 0 1em 0",
              margin: 0,
              top: 0,
              left: 0,
              right: 0,
            }}
          >
            {frame.shapes.map(
              ({ properties, shapeType }: Shape, key: number) => {
		  // If the inspector is crashing around here, then probably the shape doesn't have the width/height properties, so add a special case as below
		  // console.log("properties, shapeType", properties, shapeType, properties.w, properties.h);

                  let [w, h] = [0, 0];

                  if (shapeType === "Circle") {
		      [w, h] = 
			  [
                              (properties.r.contents as number) * 2,
                              (properties.r.contents as number) * 2,
			  ];
		  } else if (shapeType === "Square") {
		      [w, h] = 
			  [
			      (properties.side.contents as number),
			      (properties.side.contents as number),
			  ];
      } else if (shapeType === "Ellipse") {
          [w, h] = 
          [
            (properties.rx.contents as number) * 2,
            (properties.ry.contents as number) * 2,
          ];
      } else if (shapeType === "Arrow" || shapeType === "Line") {
		      const [sx, sy, ex, ey] = [properties.startX.contents as number, 
						properties.startY.contents as number, 
						properties.endX.contents as number, 
						properties.endY.contents as number];

		      const padding = 50; // Because arrow may be horizontal or vertical, and we don't want the size to be zero in that case
		      // size of bbox of arrow
		      [w, h] = [Math.max(Math.abs(ex - sx), padding), Math.max(Math.abs(ey - sy), padding)];
		  } else {
		      [w, h] = [properties.w.contents as number, properties.h.contents as number];
		  }

                  return (
                  <ShapeItem
                    key={`shapePreview-${key}`}
                    selected={selectedShape === key}
                    onClick={() => this.setSelectedShape(key)}
                  >
                    <div>
                      <svg viewBox={`0 0 ${w} ${h}`} width="50" height="50">
                        {React.createElement(staticMap[shapeType], {
                          shape: {
                            ...properties,
                            x: { tag: "FloatV", contents: 0 },
                            y: { tag: "FloatV", contents: 0 },
                          },
                          canvasSize: [w, h],
                        })}
                      </svg>
                    </div>
                    <div style={{ margin: "0.5em" }}>
                      <span>{properties.name.contents}</span>
                    </div>
                  </ShapeItem>
                );
              }
            )}
          </ul>
        </div>
        <div
          style={{
            // BUG: scroll doesnt really work
            padding: "1em 1em 1em 1em",
            overflow: "auto",
            height: "100%",
            flexGrow: 1,
            boxSizing: "border-box"
          }}
        >
          {frame.shapes[selectedShape] && (
            <ObjectInspector data={frame.shapes[selectedShape].properties} />
          )}
        </div>
      </div>
    );
  }
}

export default ShapeView;
