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
    const { instances, selectedInstance, selectedInstanceFrame } = this.props;
    const selected = instances[selectedInstance];
    if (!selected) {
      return <div />;
    }
    const frame =
      selectedInstanceFrame === -1
        ? selected[selected.length - 1]
        : selected[selectedInstanceFrame];

    const { selectedShape } = this.state;
    return (
      <div
        style={{
          display: "flex",
          width: "100%",
          height: "100%",
          overflow: "hidden",
          boxSizing: "border-box"
        }}
      >
        <div
          style={{ overflowY: "auto", height: "100%", boxSizing: "border-box" }}
        >
          <ul
            style={{
              listStyleType: "none",
              padding: "0 0 1em 0",
              margin: 0,
              top: 0,
              left: 0,
              right: 0
            }}
          >
            {frame.shapesr.map(([name, shape]: any, key: number) => {
              const [w, h] =
                name === "Circle"
                  ? [shape.r.contents * 2, shape.r.contents * 2]
                  : [shape.w.contents, shape.h.contents];
              return (
                <ShapeItem
                  key={`shapePreview-${key}`}
                  selected={selectedShape === key}
                  onClick={() => this.setSelectedShape(key)}
                >
                  <div>
                    <svg viewBox={`0 0 ${w} ${h}`} width="50" height="50">
                      {React.createElement(staticMap[name], {
                        shape: {
                          ...shape,
                          x: { tag: "FloatV", contents: 0 },
                          y: { tag: "FloatV", contents: 0 }
                        },
                        canvasSize: [w, h]
                      })}
                    </svg>
                  </div>
                  <div style={{ margin: "0.5em" }}>
                    <span>{shape.name.contents}</span>
                  </div>
                </ShapeItem>
              );
            })}
          </ul>
        </div>
        <div
          style={{
            padding: "1em 1em 2em 1em",
            overflow: "auto",
            height: "100%",
            flexGrow: 1,
            boxSizing: "border-box"
          }}
        >
          {frame.shapesr[selectedShape] && (
            <ObjectInspector data={frame.shapesr[selectedShape]} />
          )}
        </div>
      </div>
    );
  }
}

export default ShapeView;
