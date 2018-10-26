import * as React from "react";
import componentMap from "./componentMap";

interface IProps {
  data: any;
}

interface IState {
  data: any;
}

class Canvas extends React.Component<IProps, IState> {
  public readonly canvasSize: [number, number] = [800, 700];
  public readonly state = { data: [] };

  public renderEntity = ([name, shape]: [string, object], key: number) => {
    const component = componentMap[name];
    if (component === undefined) {
      return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    }
    const canvasSize = this.canvasSize;
    return React.createElement(component, { key, shape, canvasSize });
  };
  public render() {
    const { data } = this.props;
    if (data.length === undefined) {
      return <svg />;
    }
    return (
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width={this.canvasSize[0]}
        height={this.canvasSize[1]}
      >
        {data.map(this.renderEntity)}
      </svg>
    );
  }
}

export default Canvas;
