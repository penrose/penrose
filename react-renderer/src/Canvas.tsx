import * as React from "react";
import * as ReactDOM from "react-dom";
import { interactiveMap, staticMap } from "./componentMap";
import Log from "./Log";
import { loadImages } from "./Util";
import { ILayer, ILayerProps } from "./types";
import { layerMap } from "./layers/layerMap";
import { insertPending } from "./PropagateUpdate";
import { collectLabels } from "./utills/CollectLabels";
import { evalTranslation, decodeState } from "./Evaluator";

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
  public static sortShapes = (shapes: Shape[], ordering: string[]) => {
    return ordering.map((name) =>
      shapes.find(({ properties }) => properties.name.contents === name)
    ); // assumes that all names are unique
  };

  public static notEmptyLabel = ({ properties }: any) => {
    return name === "Text" ? !(properties.string.contents === "") : true;
  };

  public static processData = async (data: any) => {
    const state: State = evalTranslation(decodeState(data));
    const labeledShapes: any = await collectLabels(state.shapes);
    const labeledShapesWithImgs: any = await loadImages(labeledShapes);

    const sortedShapes: any = await Canvas.sortShapes(
      labeledShapesWithImgs,
      data.shapeOrdering
    );

    const nonEmpties = await sortedShapes.filter(Canvas.notEmptyLabel);
    const processed = await insertPending({
      ...state,
      shapes: nonEmpties,
    });
    return processed;
  };

  /**
   * Hard-coded canvas size
   * @type {[number, number]}
   * @memberof Canvas
   */
  public readonly canvasSize: [number, number] = [800, 700];
  // public readonly canvasSize: [number, number] = [400, 400];
  public readonly svg = React.createRef<SVGSVGElement>();

  /**
   * Retrieve data from drag events and update varying state accordingly
   * @memberof Canvas
   */
  public dragEvent = async (id: string, dx: number, dy: number) => {
    if (this.props.updateData) {
      const updated = {
        ...this.props.data,
        paramsr: { ...this.props.data.paramsr, optStatus: { tag: "NewIter" } },
        shapesr: this.props.data.shapesr.map(
          ([type, properties]: [string, any]) => {
            if (properties.name.contents === id) {
              return this.dragShape([type, properties], dx, dy);
            }
            return [type, properties];
          }
        ),
      };
      // TODO: need to retrofit this implementation to the new State type
      // const updatedWithVaryingState = await updateVaryingState(updated);
      // this.props.updateData(updatedWithVaryingState);
    }
  };

  public dragShape = ([type, properties]: any, dx: number, dy: number) => {
    switch (type) {
      case "Curve":
        console.log("Curve drag unimplemented", [type, properties]); // Just to prevent crashing on accidental drag
        return [type, properties];
      case "Line":
        return [
          type,
          this.moveProperties(properties, [
            ["startX", dx],
            ["startY", dy],
            ["endX", dx],
            ["endY", dy],
          ]),
        ];
      case "Arrow":
        return [
          type,
          this.moveProperties(properties, [
            ["startX", dx],
            ["startY", dy],
            ["endX", dx],
            ["endY", dy],
          ]),
        ];
      default:
        return [
          type,
          this.moveProperties(properties, [
            ["x", dx],
            ["y", dy],
          ]),
        ];
    }
  };

  /**
   * For each of the specified properties listed in `propPairs`, subtract a number from the original value.
   *
   * @memberof Canvas
   */
  public moveProperties = (properties: any, propPairs: [string, number][]) => {
    const moveProperty = (props: any, [propertyID, n]: [string, number]) => {
      props[propertyID].contents -= n;
      return props;
    };
    return propPairs.reduce(moveProperty, properties);
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
          const transform = image.getAttribute("transform");

          const wrapper = document.createElement("div");
          wrapper.innerHTML = contents;

          const s = wrapper.getElementsByTagName("svg")[0];
          s.setAttributeNS(null, "width", width);
          s.setAttributeNS(null, "height", height);
          const outer = s.outerHTML;
          const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
          g.innerHTML = outer;
          g.setAttributeNS(
            null,
            "transform",
            `${transform} translate(${x},${y})`
          );
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
      type: "image/svg+xml;charset=utf-8",
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

  public renderEntity = ({ shapeType, properties }: any, key: number) => {
    const component = this.props.lock
      ? staticMap[shapeType]
      : interactiveMap[shapeType];
    if (component === undefined) {
      Log.error(`Could not render GPI ${shapeType}.`);
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
      shape: properties,
      canvasSize,
      dragEvent,
      ctm: !this.props.lock ? (this.svg.current as any).getScreenCTM() : null,
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
      canvasSize: this.canvasSize,
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
      style,
    } = this.props;

    if (!data) {
      return <svg ref={this.svg} />;
    }

    const { shapes } = data;
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
        {shapes.map(this.renderEntity)}
        {layers &&
          layers.map(({ layer, enabled }: ILayer, key: number) => {
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
    );
  }
}

export default Canvas;
