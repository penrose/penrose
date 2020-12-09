import { updateVaryingValues } from "engine/PropagateUpdate";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { interactiveMap, staticMap } from "shapes/componentMap";
import Log from "utils/Log";

interface ICanvasProps {
  lock: boolean;
  substanceMetadata?: string;
  styleMetadata?: string;
  elementMetadata?: string;
  otherMetadata?: string;
  style?: any;
  penroseVersion?: string;
  data: State | undefined;
  updateData?: (shapes: any, step?: boolean) => void;
}

/**
 * Hard-coded canvas size
 * @type {[number, number]}
 */
export const canvasSize: [number, number] = [800, 700];

class Canvas extends React.Component<ICanvasProps> {
  // public readonly canvasSize: [number, number] = [400, 400];
  public readonly svg = React.createRef<SVGSVGElement>();

  /**
   * Retrieve data from drag events and update varying state accordingly
   * @memberof Canvas
   */
  public dragEvent = (id: string, dx: number, dy: number): void => {
    if (this.props.updateData && this.props.data) {
      const updated: State = {
        ...this.props.data,
        params: { ...this.props.data.params, optStatus: { tag: "NewIter" } },
        shapes: this.props.data.shapes.map(
          ({ shapeType, properties }: Shape) => {
            if (properties.name.contents === id) {
              return this.dragShape({ shapeType, properties }, [dx, dy]);
            }
            return { shapeType, properties };
          }
        ),
      };
      // TODO: need to retrofit this implementation to the new State type
      const updatedWithVaryingState = updateVaryingValues(updated);
      this.props.updateData(updatedWithVaryingState);
    }
  };

  // TODO: factor out position props in shapedef
  public dragShape = (shape: Shape, offset: [number, number]): Shape => {
    const { shapeType, properties } = shape;
    switch (shapeType) {
      case "Path":
        console.log("Path drag unimplemented", shape); // Just to prevent crashing on accidental drag
        return shape;
      case "Line":
        return {
          ...shape,
          properties: this.moveProperties(properties, ["start", "end"], offset),
        };
      case "Arrow":
        return {
          ...shape,
          properties: this.moveProperties(properties, ["start", "end"], offset),
        };
      default:
        return {
          ...shape,
          properties: this.moveProperties(properties, ["center"], offset),
        };
    }
  };

  /**
   * For each of the specified properties listed in `propPairs`, subtract a number from the original value.
   *
   * @memberof Canvas
   */
  public moveProperties = (
    properties: Properties,
    propsToMove: string[],
    [dx, dy]: [number, number]
  ): Properties => {
    const moveProperty = (props: Properties, propertyID: string) => {
      const [x, y] = props[propertyID].contents as [number, number];
      props[propertyID].contents = [x + dx, y + dy];
      return props;
    };
    return propsToMove.reduce(moveProperty, properties);
  };

  public prepareSVGContent = async () => {
    const domnode = ReactDOM.findDOMNode(this);
    if (domnode !== null && domnode instanceof Element) {
      const exportingNode = domnode.cloneNode(true) as any;
      exportingNode.setAttribute("width", canvasSize[0].toString());
      exportingNode.setAttribute("height", canvasSize[1].toString());

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

  public downloadState = () => {
    const content = JSON.stringify({ ...this.props.data, params: {} });
    const blob = new Blob([content], {
      type: "text/plain",
    });
    const url = URL.createObjectURL(blob);
    const downloadLink = document.createElement("a");
    downloadLink.href = url;
    downloadLink.download = `state.json`;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
  };

  componentDidMount() {
    this.forceUpdate();
  }

  public renderGPI = (
    { shapeType, properties }: Shape,
    labels: LabelCache,
    key: number
  ) => {
    const component = this.props.lock
      ? staticMap[shapeType]
      : interactiveMap[shapeType];
    if (component === undefined) {
      Log.error(`Could not render GPI ${shapeType}.`);
      return <rect fill="red" x={0} y={0} width={100} height={100} key={key} />;
    }
    if (!this.props.lock && this.svg.current === null) {
      return <g key={key}>Pending</g>;
    }
    const { dragEvent } = this;
    return React.createElement(component, {
      key,
      shape: properties,
      labels,
      canvasSize,
      dragEvent,
      ctm: !this.props.lock ? this.svg.current?.getScreenCTM() : null,
      // ctm: { a: 1, b: 0, c: 0, d: 1, e: 0, f: 20.511363191791812 },
    });
  };

  public render() {
    const {
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

    const { shapes, labelCache } = data;

    return (
      <svg
        xmlns="http://www.w3.org/2000/svg"
        version="1.2"
        width="100%"
        height="100%"
        style={style || {}}
        ref={this.svg}
        viewBox={`0 0 ${canvasSize[0]} ${canvasSize[1]}`}
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
        {shapes && shapes.map((s, k) => this.renderGPI(s, labelCache, k))}
      </svg>
    );
  }
}

export default Canvas;
