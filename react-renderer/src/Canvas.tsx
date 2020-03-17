import * as React from "react";
import * as ReactDOM from "react-dom";
import { interactiveMap, staticMap } from "./componentMap";
import Log from "./Log";
import { loadImages } from "./Util";
import { ILayer, ILayerProps } from "./types";
import { layerMap } from "./layers/layerMap";
import { propagateUpdate, updateVaryingState } from "./PropagateUpdate";
import { collectLabels } from "./utills/CollectLabels";

interface IProps {
  lock: boolean;
  layers?: ILayer[];
  substanceMetadata?: string;
  styleMetadata?: string;
  elementMetadata?: string;
  otherMetadata?: string;
  style?: any;
  penroseVersion?: string;
  data: any;
  updateData?: (shapes: any, step?: boolean) => void;
}

class Canvas extends React.Component<IProps> {
  public static sortShapes = (shapes: any[], ordering: string[]) => {
    return ordering.map(name =>
      shapes.find(([_, shape]) => shape.name.contents === name)
    ); // assumes that all names are unique
  };

  public static notEmptyLabel = ([name, shape]: [string, any]) => {
    return name === "Text" ? !(shape.string.contents === "") : true;
  };

  public static processData = async (data: any) => {
    if (!data.shapesr) {
      return {};
    }
    const shapes = data.shapesr;
    const labeledShapes = await collectLabels(shapes);
    const labeledShapesWithImgs = await loadImages(labeledShapes);

    const sortedShapes = await Canvas.sortShapes(
      labeledShapesWithImgs,
      data.shapeOrdering
    );

    const nonEmpties = await sortedShapes.filter(Canvas.notEmptyLabel);
    const processed = await propagateUpdate({
      ...data,
      shapesr: nonEmpties
    });
    return processed;
  };
  public readonly canvasSize: [number, number] = [800, 700];
  public readonly svg = React.createRef<SVGSVGElement>();

  public dragEvent = async (id: string, dy: number, dx: number) => {
    if (this.props.updateData) {
      const updated = await propagateUpdate({
        ...this.props.data,
        paramsr: { ...this.props.data.paramsr, optStatus: { tag: "NewIter" } },
        shapesr: this.props.data.shapesr.map(
          ([shapeType, shape]: [string, any]) => {
            if (shape.name.contents === id) {
              if (shapeType === "Curve") {
                console.log("Curve drag unimplemented", shape); // Just to prevent crashing on accidental drag
                return [
                  shapeType,
                  { ...shape } // TODO: need to map (-dx, -dy) over all the path pieces
                ];
              }

              return [
                shapeType,
                {
                  ...shape,
                  x: { ...shape.x, contents: shape.x.contents - dx },
                  y: { ...shape.y, contents: shape.y.contents - dy }
                }
              ];
            }

            return [shapeType, shape];
          }
        )
      });
      const updatedWithVaryingState = await updateVaryingState(updated);
      this.props.updateData(updatedWithVaryingState);
    }
  };

  public prepareSVGContent = async () => {
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
    const content = await this.prepareSVGContent();
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

  public getRawSVG = async () => {
    const content = await this.prepareSVGContent();
    return content;
  };

  public downloadPDF = async () => {
    const content = await this.prepareSVGContent();
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
    const component = this.props.lock ? staticMap[name] : interactiveMap[name];
    if (component === undefined) {
      Log.error(`Could not render GPI ${name}.`);
      return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    }
    if (!this.props.lock && this.svg.current === null) {
      Log.error("SVG ref is null");
      return <g key={key}>broken!</g>;
    }
    const canvasSize = this.canvasSize;
    const { dragEvent } = this;
    return React.createElement(component, {
      key,
      shape,
      canvasSize,
      dragEvent,
      ctm: !this.props.lock ? (this.svg.current as any).getScreenCTM() : null
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
      layers,
      substanceMetadata,
      styleMetadata,
      elementMetadata,
      otherMetadata,
      data,
      penroseVersion,
      style
    } = this.props;
    const { shapesr } = data;

    if (!shapesr) {
      return <svg ref={this.svg} />;
    }

    return (
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
          {`This diagram was created with Penrose (https://penrose.ink)${
            penroseVersion ? " version " + penroseVersion : ""
          } on ${new Date()
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
        {shapesr.map(this.renderEntity)}
        {layers &&
          layers.map(({ layer, enabled }: ILayer, key: number) => {
            if (layerMap[layer] === undefined) {
              Log.error(`Layer does not exist in deck: ${layer}`);
              return null;
            }
            if (enabled) {
              return this.renderLayer(shapesr, data, layerMap[layer], key);
            }
            return null;
          })}
      </svg>
    );
  }
}

export default Canvas;
