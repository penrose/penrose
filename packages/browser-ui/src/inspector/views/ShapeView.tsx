import * as React from "react";
import IViewProps from "./IViewProps";
import { ObjectInspector } from "react-inspector";
import makeViewBoxes from "inspector/makeViewBoxes";

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
        {makeViewBoxes(frame.shapes, selectedShape, this.setSelectedShape)}
        <div
          style={{
            // BUG: scroll doesnt really work
            padding: "1em 1em 1em 1em",
            overflow: "auto",
            height: "100%",
            flexGrow: 1,
            boxSizing: "border-box",
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
