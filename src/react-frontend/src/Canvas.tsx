import * as React from "react";
import Circle from "./Circle";

interface IProps {
  data: any;
}

interface IState {
  data: any;
}

class Canvas extends React.Component<IProps, IState> {
  public readonly canvasSize: [number, number] = [800, 700];
  public readonly state = { data: [] };

  // public getDerivedStateFromProps(props: IProps, state: IState) {
  // if () { }
  // return
  // }
  public render() {
    const { data } = this.props;
    if (data.length === undefined) {
      return <svg />;
    }
    return (
      <svg width={this.canvasSize[0]} height={this.canvasSize[1]}>
        {data.map((shape: any, index: number) => {
          const props = shape[1];
          // TODO: factor out the switch statement
          switch (shape[0]) {
            case "Circle":
              // TODO: circle react component in separate file
              return (
                <Circle
                  key={index}
                  shape={props}
                  canvasSize={this.canvasSize}
                />
              );
              break;
            default:
              return (
                <rect
                  fill="red"
                  x={0}
                  y={0}
                  width={100}
                  height={100}
                  key={index}
                />
              );
              break;
          }
        })}
      </svg>
    );
  }
}

export default Canvas;
