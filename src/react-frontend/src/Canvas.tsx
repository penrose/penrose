import * as React from "react";

interface IProps {
  data: any;
}

interface IState {
  data: any;
}

const gv = (thing: any) => thing.contents;

class Canvas extends React.Component<IProps, IState> {
  public readonly canvasSize = [800, 700];
  public readonly state = { data: {} };
  public toScreen = ([x, y]: [number, number]) => {
    const [width, height] = this.canvasSize;
    return [width / 2 + x, height / 2 - y];
  };
  public toHex = (rgba: [number, number, number, number]) => {
    return rgba.slice(0, 3).reduce((prev, cur) => {
      const hex = Math.round(255 * cur).toString(16);
      const padded = hex.length === 1 ? "0" + hex : hex;
      return prev + padded;
    }, "#");
  };
  public getDerivedStateFromProps(props: IProps, state: IState) {
    // if () { }
    // return
  }
  //   public onStart = ()
  public render() {
    const { data } = this.props;
    // tslint:disable-next-line:no-console
    // console.log(data);
    if (data.contents === undefined) {
      return <svg />;
    }
    return (
      <svg width={this.canvasSize[0]} height={this.canvasSize[1]}>
        {data.contents.shapes.map((shape: any, index: number) => {
          const props = shape[1];
          switch (shape[0]) {
            case "Circle":
              const [x, y] = this.toScreen([gv(props.x), gv(props.y)]);
              const color = this.toHex(gv(props.color));
              const alpha = gv(props.color)[3];
              return (
                <circle
                  key={index}
                  cx={x}
                  cy={y}
                  r={gv(props.r)}
                  fill={color}
                  fillOpacity={alpha}
                />
              );
              break;
            default:
              return <circle r="0" key={index} />;
              break;
          }
        })}
      </svg>
    );
  }
}

export default Canvas;
