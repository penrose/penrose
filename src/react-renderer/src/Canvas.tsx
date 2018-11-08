import * as React from "react";
import componentMap from "./componentMap";
import Log from "./Log";

interface IProps {
  data: any;
  onShapeUpdate(shape: any): void;
  dragEvent?(id: string, dy: number, dx: number): void;
}

class Canvas extends React.Component<IProps> {
  public readonly canvasSize: [number, number] = [800, 700];

  public notEmptyLabel = ([name, shape]: [string, any], key: number) => {
    return !(name === "Text" && shape.string.contents === "");
  }

  public renderEntity = ([name, shape]: [string, object], key: number) => {
    const component = componentMap[name];
    if (component === undefined) {
      Log.error(`Could not render GPI ${name}.`);
      return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    }
    const canvasSize = this.canvasSize;
    const { onShapeUpdate, dragEvent } = this.props;
    return React.createElement(component, {
      key,
      shape,
      canvasSize,
      onShapeUpdate,
      dragEvent
    });
  };
  public render() {
    const { data } = this.props;
    if (data.length === undefined) {
      return <svg />;
    }
    return (
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="100%"
        height="100%"
        viewBox={`0 0 ${this.canvasSize[0]} ${this.canvasSize[1]}`}
      >
        {data.filter(this.notEmptyLabel).map(this.renderEntity)}
      </svg>
    );
  }
}

export default Canvas;
