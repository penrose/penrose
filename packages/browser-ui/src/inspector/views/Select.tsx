import * as React from "react";
import IViewProps from "./IViewProps";
import cytoscape from "cytoscape";
import { uniqBy } from "lodash";
import dagre from "cytoscape-dagre";
import {
  ShapeTypes,
  PenroseState,
  PenroseFn,
  prettyPrintFn,
  prettyPrintPath,
  prettyPrintExpr,
  graphOfBlockExpr,
} from "@penrose/core";
import { FieldDict, Translation } from "@penrose/core/build/dist/types/value";
import GraphForm from "./GraphForm";

const Select: React.FC<IViewProps> = ({ frame, history }: IViewProps) => {
  if (!frame) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no frame
      </div>
    );
  }

  return <GraphForm />;
};

export default Select;
