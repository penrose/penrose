import * as React from "react";
import * as ReactDOM from "react-dom";
import componentMap from "./componentMap";
import Log from "./Log";
import { LockContext } from "./contexts";
import { collectLabels, loadImages } from "./Util";
import { ILayer, ILayerProps } from "./types";
import { layerMap } from "./layers/layerMap";
import { pickBy } from "lodash";

interface IProps {
  lock: boolean;
  layers: ILayer[];
  substanceMetadata?: string;
  styleMetadata?: string;
  elementMetadata?: string;
  otherMetadata?: string;
  style?: any;
  data: any;
  updateData(instance: any): void;
}

interface IState {
  shapes: any[];
  debugData: any;
}

class Canvas extends React.Component<IProps, IState> {
  public readonly state = { shapes: [], debugData: [] };
  public readonly canvasSize: [number, number] = [800, 700];
  public readonly svg = React.createRef<SVGSVGElement>();
  public sortShapes = (shapes: any[], ordering: string[]) => {
    return ordering.map(name =>
      shapes.find(([_, shape]) => shape.name.contents === name)
    ); // assumes that all names are unique
  };

  public notEmptyLabel = ([name, shape]: [string, any]) => {
    return name === "Text" ? !(shape.string.contents === "") : true;
  };

  public onMessage = async (e: MessageEvent) => {
    const myJSON = JSON.parse(e.data).contents;
    const { flag, shapes } = myJSON;

    // For final frame
    if (flag === "final") {
      Log.info("Fully optimized.");
    }

    // Compute (or retrieve from memory) label and image dimensions (as well as the rendered DOM elements)
    const labeledShapes = await collectLabels(shapes);
    const labeledShapesWithImgs = await loadImages(labeledShapes);

    // For initial frame, send only dimensions to backend (this only sends the Image and Text GPIs)
    if (flag === "initial") {
      this.props.updateData(labeledShapesWithImgs);
    }
  };
  public async componentDidUpdate(prevProps: IProps) {
    const shapes = this.props.data.shapesr;
    const labeledShapes = await collectLabels(shapes);
    const labeledShapesWithImgs = await loadImages(labeledShapes);

    const sortedShapes = this.sortShapes(
      labeledShapesWithImgs,
      this.props.data.shapeOrdering
    );

    const nonEmpties = sortedShapes.filter(this.notEmptyLabel);
    if (nonEmpties.length !== this.state.shapes.length) {
      this.setState({ shapes: nonEmpties });
    }
    if (this.props.data.flag === "initial") {
      this.update(labeledShapesWithImgs);
    }
  }
  // IMPORTANT: componentDidUpdate: if un-rendered labels/images, do that first
  public update = (updatedShapes: any[]) => {
    this.props.updateData({
      ...this.props.data,
      shapesr: updatedShapes.map(([name, shape]: [string, any]) => [
        name,
        pickBy(shape, (k: any) => !k.omit)
      ])
    });
  };

  public dragEvent = (id: string, dy: number, dx: number) => {
    this.update(
      this.state.shapes.map(([name, shape]: [string, any]) => {
        if (shape.name.contents === id) {
          return [name, { ...shape }];
        }
        return [name, shape];
      })
    );
  };

  public download = async () => {
    const domnode = ReactDOM.findDOMNode(this);
    if (domnode !== null && domnode instanceof Element) {
      const exportingNode = domnode.cloneNode(true) as any;
      exportingNode.setAttribute("width", this.canvasSize[0].toString());
      exportingNode.setAttribute("height", this.canvasSize[1].toString());

      const images = exportingNode.getElementsByTagName("image");
      for (let i = images.length - 1; i >= 0; i--) {
        const image = images[i];
        const uri = image.getAttribute("href");
        const response = await fetch(uri);
        const contents = await response.text();
        if (response.ok) {
          const width = image.getAttribute("width");
          const height = image.getAttribute("height");
          const x = image.getAttribute("x");
          const y = image.getAttribute("y");

          const wrapper = document.createElement("div");
          wrapper.innerHTML = contents;

          const s = wrapper.getElementsByTagName("svg")[0];
          s.setAttributeNS(null, "width", width);
          s.setAttributeNS(null, "height", height);
          const outer = s.outerHTML;
          const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
          g.innerHTML = outer;
          g.setAttributeNS(null, "transform", `translate(${x},${y})`);
          // HACK: generate unique ids
          const defs = g.getElementsByTagName("defs");
          if (defs.length > 0) {
            defs[0].querySelectorAll("*").forEach((node: any) => {
              if (node.id !== "") {
                const users = g.querySelectorAll(
                  `[*|href="#${node.id}"]:not([href])`
                );
                users.forEach((user: any) => {
                  const unique = `${i}-ns-${node.id}`;
                  user.setAttributeNS(
                    "http://www.w3.org/1999/xlink",
                    "href",
                    "#" + unique
                  );
                  node.setAttribute("id", unique);
                });
              }
            });
          }
          image.insertAdjacentElement("beforebegin", g);
          wrapper.remove();
          image.remove();
        } else {
          Log.error(`Could not fetch ${uri}`);
        }
      }
      return exportingNode.outerHTML;
    } else {
      Log.error("Could not find SVG domnode.");
      return "";
    }
  };

  public downloadSVG = async (title = "illustration") => {
    const content = await this.download();
    const blob = new Blob([content], {
      type: "image/svg+xml;charset=utf-8"
    });
    const url = URL.createObjectURL(blob);
    const downloadLink = document.createElement("a");
    downloadLink.href = url;
    downloadLink.download = `${title}.svg`;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
  };

  public downloadPDF = async () => {
    const content = await this.download();
    const frame = document.createElement("iframe");
    document.body.appendChild(frame);
    const pri = frame.contentWindow;
    frame.setAttribute(
      "style",
      "height: 100%; width: 100%; position: absolute"
    );
    if (content && pri) {
      console.log("Printing pdf now...");
      pri.document.open();
      pri.document.write(content);
      pri.document.close();
      pri.focus();
      pri.print();
    }
    frame.remove();
  };

  public renderEntity = ([name, shape]: [string, object], key: number) => {
    const component = componentMap[name];
    if (component === undefined) {
      Log.error(`Could not render GPI ${name}.`);
      return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    }
    if (this.svg.current === null) {
      Log.error("SVG ref is null");
      return <g key={key} />;
    }
    const ctm = this.svg.current.getScreenCTM();
    const canvasSize = this.canvasSize;
    const { dragEvent } = this;
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
    debugData: any[],
    component: React.ComponentClass<ILayerProps>,
    key: number
  ) => {
    if (shapes.length === 0) {
      return <g key={key} />;
    }
    if (this.svg.current === null) {
      Log.error("SVG ref is null");
      return <g key={key} />;
    }
    const ctm = this.svg.current.getScreenCTM();
    if (ctm === null) {
      Log.error("Cannot get CTM");
      return <g key={key} />;
    }
    return React.createElement(component, {
      key,
      ctm,
      shapes,
      debugData,
      canvasSize: this.canvasSize
    });
  };
  public render() {
    const {
      lock,
      layers,
      substanceMetadata,
      styleMetadata,
      elementMetadata,
      otherMetadata,
      data,
      style
    } = this.props;
    const { shapes } = this.state;

    if (shapes === []) {
      return <svg />;
    }
    return (
      <LockContext.Provider value={lock}>
        <svg
          xmlns="http://www.w3.org/2000/svg"
          version="1.2"
          width="100%"
          height="100%"
          style={style || {}}
          ref={this.svg}
          viewBox={`0 0 ${this.canvasSize[0]} ${this.canvasSize[1]}`}
        >
          <desc>
            {`This diagram was created with Penrose (https://penrose.ink) on ${new Date()
              .toISOString()
              .slice(
                0,
                10
              )}. If you have any suggestions on making this diagram more accessible, please contact us.\n`}
            {substanceMetadata && `${substanceMetadata}\n`}
            {styleMetadata && `${styleMetadata}\n`}
            {elementMetadata && `${elementMetadata}\n`}
            {otherMetadata && `${otherMetadata}`}
          </desc>
          {shapes.map(this.renderEntity)}
          {layers.map(({ layer, enabled }: ILayer, key: number) => {
            if (layerMap[layer] === undefined) {
              Log.error(`Layer does not exist in deck: ${layer}`);
              return null;
            }
            if (enabled) {
              return this.renderLayer(shapes, data, layerMap[layer], key);
            }
            return null;
          })}
        </svg>
      </LockContext.Provider>
    );
  }
}

export default Canvas;
