import * as React from "react";
import * as ReactDOM from "react-dom";
import componentMap from "./componentMap";
import Log from "./Log";
import { LockContext } from "./contexts";
import { collectLabels } from "./Util";
import {drag, update} from "./packets";

interface IProps {
  lock: boolean;
  sendPacket(packet: string): void;
}

interface IState {
  data: any;
}

class Canvas extends React.Component<IProps, IState> {
  public readonly state = { data: [] };
  public readonly canvasSize: [number, number] = [800, 700];
  public readonly svg = React.createRef<SVGSVGElement>();

  public notEmptyLabel = ([name, shape]: [string, any]) => {
    return !(name === "Text" && shape.string.contents === "");
  };
  public onMessage = async (e: MessageEvent) => {
    let myJSON = JSON.parse(e.data).contents;
    const flag = myJSON.flag;
    if (flag) {
      myJSON = myJSON.shapes;
    }
    // For final frame
    if (flag && flag === "final") {
      Log.info("Fully optimized.");
    }
    // Compute (or retrieve from memory) label dimensions
    const results = await collectLabels(myJSON);

    // For initial frame - send dimensions
    if (flag && flag === "initial") {
      this.sendUpdate(results);
    }
    this.setState({ data: results });
  };
  public dragEvent = (id: string, dy: number, dx: number) => {
    this.props.sendPacket(drag(id, dy, dx));
  };

  public sendUpdate = (updatedShapes: any[]) => {
    Log.info("Sending an Update packet to the server...");
    this.props.sendPacket(update(updatedShapes));
  };

  public onShapeUpdate = (updatedShape: any) => {
    const shapes = this.state.data.map(([name, oldShape]: [string, any]) => {
      if (oldShape.name.contents === updatedShape.name.contents) {
        return [name, updatedShape];
      }
      return [name, oldShape];
    });
    this.setState({ data: shapes });
    this.sendUpdate(shapes);
  };

  public download = () => {
    const domnode = ReactDOM.findDOMNode(this);
    if (domnode !== null && domnode instanceof Element) {
      domnode.setAttribute("width", this.canvasSize[0].toString());
      domnode.setAttribute("height", this.canvasSize[1].toString());
      const blob = new Blob([domnode.outerHTML], {
        type: "image/svg+xml;charset=utf-8"
      });
      const url = URL.createObjectURL(blob);
      const downloadLink = document.createElement("a");
      downloadLink.href = url;
      downloadLink.download = "illustration.svg";
      document.body.appendChild(downloadLink);
      downloadLink.click();
      document.body.removeChild(downloadLink);
      domnode.setAttribute("width", "100%");
      domnode.setAttribute("height", "100%");
    } else {
      Log.error("Could not find SVG domnode.");
    }
  };
  public renderEntity = ([name, shape]: [string, object], key: number) => {
    const component = componentMap[name];
    if (component === undefined) {
      Log.error(`Could not render GPI ${name}.`);
      return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    }
    if (this.svg.current === null) {
      Log.error("SVG ref is null");
      return <rect />;
    }
    const ctm = this.svg.current.getScreenCTM();
    const canvasSize = this.canvasSize;
    const { onShapeUpdate, dragEvent } = this;
    return React.createElement(component, {
      key,
      shape,
      canvasSize,
      onShapeUpdate,
      dragEvent,
      ctm
    });
  };
  public render() {
    const { lock } = this.props;
    const { data } = this.state;
    if (data.length === undefined) {
      return <svg />;
    }
    return (
      <LockContext.Provider value={lock}>
        <svg
          xmlns="http://www.w3.org/2000/svg"
          version="1.2"
          width="100%"
          height="100%"
          ref={this.svg}
          viewBox={`0 0 ${this.canvasSize[0]} ${this.canvasSize[1]}`}
        >
          {data.filter(this.notEmptyLabel).map(this.renderEntity)}
        </svg>
      </LockContext.Provider>
    );
  }
}

export default Canvas;
