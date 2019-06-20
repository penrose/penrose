import * as React from "react";
import * as ReactDOM from "react-dom";
import componentMap from "./componentMap";
import Log from "./Log";
import { LockContext } from "./contexts";
import { collectLabels, loadImages } from "./Util";
import { ILayer, ILayerProps } from "./types";
import { layerMap } from "./layers/layerMap";
import { isEqual, mapValues } from "lodash";

interface IProps {
  lock: boolean;
  layers: ILayer[];
  substanceMetadata?: string;
  styleMetadata?: string;
  elementMetadata?: string;
  otherMetadata?: string;
  style?: any;
  data: any;
  updateData(shapes: any): void;
}

class Canvas extends React.Component<IProps> {
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
  public propagateUpdate = async (data: any) => {
    const updatedTranslation = await data.transr.trMap.map(
      ([fst, tr]: [any, any]) => {
        return [
          fst,
          mapValues(tr, (val: any) => {
            if (val.tag === "FGPI") {
              const shapeContent = data.shapesr.filter(
                ([__, shape]: [string, any]) => {
                  return (
                    shape.name.contents ===
                    val.contents[1].name.contents.contents
                  );
                }
              )[0][1];
              const newVal = { ...val };
              const updateContents = (key: string) => {
                if (key in newVal.contents[1]) {
                  newVal.contents[1][key].contents.contents =
                    shapeContent[key].contents;
                }
              };
              updateContents("w");
              updateContents("h");
              updateContents("x");
              updateContents("y");
              return newVal;
            }
            return val;
          })
        ];
      }
    );
    const newVaryingState = [...data.varyingState];
    await data.varyingPaths.forEach((path: any, index: number) => {
      updatedTranslation.forEach(([fst, snd]: [any, any], trIndex: number) => {
        if (fst.contents === path.contents[0].contents) {
          newVaryingState[index] =
            snd[path.contents[1]].contents[1][
              path.contents[2]
            ].contents.contents;
          updatedTranslation[trIndex][1] = snd;
        }
      });
    });
    return {
      ...data,
      varyingState: newVaryingState,
      transr: { ...data.transr, trMap: updatedTranslation },
      paramsr: { ...data.paramsr, optStatus: { tag: "NewIter" } }
    };
  };

  public async componentDidUpdate(prevProps: IProps) {
    if (!isEqual(prevProps.data, this.props.data)) {
      const shapes = this.props.data.shapesr;
      const labeledShapes = await collectLabels(shapes);
      const labeledShapesWithImgs = await loadImages(labeledShapes);

      const sortedShapes = await this.sortShapes(
        labeledShapesWithImgs,
        this.props.data.shapeOrdering
      );

      const nonEmpties = await sortedShapes.filter(this.notEmptyLabel);

      await this.props.updateData({ ...this.props.data, shapesr: nonEmpties });
    }
  }
  public dragEvent = async (id: string, dy: number, dx: number) => {
    const updated = await this.propagateUpdate({
      ...this.props.data,
      shapesr: this.props.data.shapesr.map(([name, shape]: [string, any]) => {
        if (shape.name.contents === id) {
          return [
            name,
            {
              ...shape,
              x: { ...shape.x, contents: shape.x.contents - dx },
              y: { ...shape.y, contents: shape.y.contents - dy }
            }
          ];
        }
        return [name, shape];
      })
    });
    this.props.updateData(updated);
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
