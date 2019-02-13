import * as React from "react";
import * as ReactDOM from "react-dom";
import componentMap from "./componentMap";
import Log from "./Log";
import { LockContext } from "./contexts";
import { collectLabels } from "./Util";
import {drag, update} from "./packets";
import {ILayer, ILayerProps} from "./types";
import {layerMap} from "./layers/layerMap";

interface IProps {
  lock: boolean;
  layers: ILayer[];
  sendPacket(packet: string): void;
}

interface IState {
  data: any;
}

class Canvas extends React.Component<IProps, IState> {
  public readonly state = { data: [] };
  public readonly canvasSize: [number, number] = [800, 700];
  public readonly svg = React.createRef<SVGSVGElement>();
  public sortShapes = (shapes: any[], ordering: string[]) => {
    return ordering.map(name =>
      shapes.find(([_, shape]) => shape.name.contents === name)
    ); // assumes that all names are unique
  };

  public notEmptyLabel = ([name, shape]: [string, any]) => {
    return !(name === "Text" && shape.string.contents === "");
  };

  public onMessage = async (e: MessageEvent) => {
    const myJSON = JSON.parse(e.data).contents;
    const { flag, shapes, ordering } = myJSON;
    // For final frame
    if (flag === "final") {
      Log.info("Fully optimized.");
    }
    // Compute (or retrieve from memory) label dimensions
    const labeledShapes = await collectLabels(shapes);
    // For initial frame - send dimensions
    if (flag === "initial") {
      this.sendUpdate(labeledShapes);
    }
    this.setState({ data: this.sortShapes(labeledShapes, ordering) });
  };

  public dragEvent = (id: string, dy: number, dx: number) => {
    this.props.sendPacket(drag(id, dy, dx));
  };

  public sendUpdate = (updatedShapes: any[]) => {
    Log.info("Sending an Update packet to the server...");
    this.props.sendPacket(update(updatedShapes));
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
      return <g key={key}/>;
    }
    const ctm = this.svg.current.getScreenCTM();
    const canvasSize = this.canvasSize;
    const {dragEvent} = this;
    return React.createElement(component, {
      key,
      shape,
      canvasSize,
      dragEvent,
      ctm
    });
  };
  public renderLayer = (
    shapes: Array<[string, object]>,
    component: React.ComponentClass<ILayerProps>,
    key: number
  ) => {
    if (shapes.length === 0) {
      return <g key={key}/>;
    }
    if (this.svg.current === null) {
      Log.error("SVG ref is null");
      return <g key={key}/>;
    }
    const ctm = this.svg.current.getScreenCTM();
    if (ctm === null) {
      Log.error("Cannot get CTM");
      return <g key={key}/>;
    }
    return React.createElement(component, {
      key,
      ctm,
      shapes,
      canvasSize: this.canvasSize
    });
  };
  public render() {
    const {lock, layers} = this.props;
    const { data } = this.state;
    if (data.length === undefined) {
      return <svg />;
    }
    const nonEmpties = data.filter(this.notEmptyLabel);
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
          {nonEmpties.map(this.renderEntity)}
          {layers.map(({layer, enabled}: ILayer, key: number) => {
            if (layerMap[layer] === undefined) {
              Log.error(`Layer does not exist in deck: ${layer}`);
              return null;
            }
            if (enabled) {
              return this.renderLayer(nonEmpties, layerMap[layer], key);
            }
            return null;
          })}
        </svg>
      </LockContext.Provider>
    );
  }
}

export default Canvas;
