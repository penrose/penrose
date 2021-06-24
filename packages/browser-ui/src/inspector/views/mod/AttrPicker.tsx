import * as React from "react";
import LabeledInput from "./LabeledInput";
import { Canvas, ShapeTypes } from "@penrose/core";

interface IProps {
  shape: ShapeTypes.Shape;
  sAttrs: IShapeDef;
  modAttr(attrname: string, attrval: ShapeTypes.Value<any>): void;
  canvas: Canvas;
}

interface IShapeDef {
  shapeType: string;
  properties: any;
}

// Q - should this update shape properties in state? not really necessary functionally but maybe ideologically
class AttrPicker extends React.Component<IProps> {
  public render() {
    const { sAttrs, shape, modAttr, canvas } = this.props;
    if (!sAttrs.hasOwnProperty("properties")) {
      throw new Error("JSON missing the 'properties' attribute.");
    }
    return (
      <div
        id="attrPicker"
        style={{ display: "flex", flexWrap: "wrap", flexDirection: "row" }}
      >
        {Object.keys(sAttrs.properties).map((ppty: string) => (
          <LabeledInput
            key={`inp-${ppty}`}
            modAttr={modAttr}
            inputProps={sAttrs.properties[ppty]}
            eAttr={ppty}
            eValue={shape.properties[ppty]}
            canvas={canvas}
          />
        ))}
      </div>
    );
  }
}

export default AttrPicker;
