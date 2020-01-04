import * as React from "react";
import * as ReactDOM from "react-dom";
import componentMap from "./componentMap";
import Log from "./Log";
import { LockContext } from "./contexts";
import { collectLabels, loadImages } from "./Util";
import { ILayer, ILayerProps } from "./types";
import { layerMap } from "./layers/layerMap";

interface IProps {
  lock: boolean;
  layers: ILayer[];
  substanceMetadata?: string;
  styleMetadata?: string;
  elementMetadata?: string;
  otherMetadata?: string;
  style?: any;
  penroseVersion?: string;
  data: any;
  updateData(shapes: any, step?: boolean): void;
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

  // helper for finding a shape by name
  public static findShapeProperty = (
    shapes: any,
    name: string,
    property: string
  ) =>
    shapes.find((shape: any) => shape[1].name.contents === name)[1][property];

  // helper for updating a pending property given a path
  public static updateProperty = (translation: any, shapes: any, path: any) => {
    const [subName, fieldName, propertyName] = path.contents;
    if (path.tag === "PropertyPath") {
      return {
        ...translation,
        trMap: translation.trMap.map(([sub, fieldDict]: [any, any]) => {
          // match substance name
          if (sub.contents === subName.contents) {
            // TODO: functional-style map on objects doesn't seem to be supported by TS well. Write helpfer?
            const updatedFieldDict = { ...fieldDict };
            for (const field of Object.keys(fieldDict)) {
              const {
                contents: [, propertyDict]
              } = fieldDict[field];
              // match field name
              if (field === fieldName) {
                // shape name is a done value of type string, hence the two accesses
                const shapeName = propertyDict.name.contents.contents;
                // find property and updated value
                const propWithUpdate = Canvas.findShapeProperty(
                  shapes,
                  shapeName,
                  propertyName
                );
                // update the property in the shape list
                propWithUpdate.contents = propWithUpdate.updated;
                const { tag, contents } = propWithUpdate;
                delete propWithUpdate.updated;

                // update the pending property in the translated by a Done value retrieved from the shapes, which are already updated (in the two lines above)
                propertyDict[propertyName] = {
                  tag: "Done",
                  contents: { tag, contents }
                };
              }
            }
            return [sub, updatedFieldDict];
          } else {
            return [sub, fieldDict];
          }
        })
      };
    } else {
      Log.error("Pending field paths are not supported");
    }
  };

  public static insertPending = async (data: any) => {
    return {
      ...data,
      // clear up pending paths now that they are updated properly
      pendingPaths: [],
      // for each of the pending path, update the translation using the updated shapes with new label dimensions etc.
      transr: data.pendingPaths.reduce(
        (trans: any, path: any) =>
          Canvas.updateProperty(data.transr, data.shapesr, path),
        data.transr
      )
    };
  };

  /**
   * Update the varying state with values from `shapes`
   *
   * @static
   * @memberof Canvas
   */
  public static updateVaryingState = async (data: any) => {
    const newVaryingState = [...data.varyingState];
    await data.varyingPaths.forEach((path: any, index: number) => {
      // NOTE: We only update property paths since no frontend interactions can change fields
      // TODO: add a branch for `FieldPath` when this is no longer the case
      if (path.tag === "PropertyPath") {
        const [{ contents: subName }, fieldName, propertyName] = path.contents;
        data.transr.trMap.forEach(
          ([subVar, fieldDict]: [any, any], fieldIndex: number) => {
            if (subVar.contents === subName) {
              const propertyDict = fieldDict[fieldName].contents[1];
              const shapeName = propertyDict.name.contents.contents;
              newVaryingState[index] = Canvas.findShapeProperty(
                data.shapesr,
                shapeName,
                propertyName
              ).contents;
            }
          }
        );
      }
    });
    return {
      ...data,
      varyingState: newVaryingState
    };
  };

  /**
   * process a packet from the backend to (1) render the initial state and (2) update the state of the renderer.
   * @memberof Canvas
   */
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
    const processed = await Canvas.insertPending({
      ...data,
      shapesr: nonEmpties
    });
    return processed;
  };

  /**
   * Hard-coded canvas size
   * @type {[number, number]}
   * @memberof Canvas
   */
  // public readonly canvasSize: [number, number] = [800, 700];
  public readonly canvasSize: [number, number] = [400, 400];
  public readonly svg = React.createRef<SVGSVGElement>();

  /**
   * Retrieve data from drag events and update varying state accordingly
   * @memberof Canvas
   */
  public dragEvent = async (id: string, dx: number, dy: number) => {
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
      )
    };
    const updatedWithVaryingState = await Canvas.updateVaryingState(updated);
    this.props.updateData(updatedWithVaryingState);
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
            ["endY", dy]
          ])
        ];
      case "Arrow":
        return [
          type,
          this.moveProperties(properties, [
            ["startX", dx],
            ["startY", dy],
            ["endX", dx],
            ["endY", dy]
          ])
        ];
      default:
        return [type, this.moveProperties(properties, [["x", dx], ["y", dy]])];
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
      penroseVersion,
      style
    } = this.props;
    const { shapesr } = data;

    if (!shapesr) {
      return <svg ref={this.svg} />;
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
          {layers.map(({ layer, enabled }: ILayer, key: number) => {
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
      </LockContext.Provider>
    );
  }
}

export default Canvas;
