import { Canvas, Shape, Value } from "@penrose/core";
import * as React from "react";
import LabeledInput from "./LabeledInput";

interface Props {
  shape: Shape;
  sAttrs: ShapeDef;
  modAttr(attrname: string, attrval: Value.Value<any>): void;
  canvas: Canvas;
}

interface ShapeDef {
  shapeType: string;
  properties: any;
}

// Q - should this update shape properties in state? not really necessary functionally but maybe ideologically
class AttrPicker extends React.Component<Props> {
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
