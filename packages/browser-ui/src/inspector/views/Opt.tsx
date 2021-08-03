import * as React from "react";
import IViewProps from "./IViewProps";
import { prettyPrintFn, evalFns, ops } from "@penrose/core";
import { zipWith } from "lodash";
import DataTable from "react-data-table-component";
import { ObjectInspector } from "react-inspector";

export const EPS = 10e-3;

const Opt: React.FC<IViewProps> = ({ frame, history }: IViewProps) => {
  if (!frame) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no frame
      </div>
    );
  }

  const constrInfos = zipWith(
    frame.constrFns.map(prettyPrintFn),
    evalFns(frame.constrFns, frame),
    (name, fnEvaled) => {
      const energy = Math.max(fnEvaled.f, 0);
      return {
        name,
        energy,
        sat: energy <= EPS ? "yes" : "no",
      };
    }
  );
  const objInfos = zipWith(
    frame.objFns.map(prettyPrintFn),
    evalFns(frame.objFns, frame),
    (name, fnEvaled) => {
      const gradientNorm = ops.vnorm(fnEvaled.gradf);
      return {
        name,
        gradientNorm,
      };
    }
  );

  const optDebugInfo = frame.optDebugInfo;

  const exp = x => x ? x.toExponential(3) : -1;
  const exp1 = xs => xs ? xs.map(x => x.toExponential(1)) : [];

  const optDebugInfoRows = zipWith(
    ["Number of steps per display step",
      "Energy",
      "Gradient norm",
      "Preconditioned gradient norm",
      "Line search step size"],
    [optDebugInfo.numSteps,
    exp(optDebugInfo.energy),
    exp(optDebugInfo.normGrad),
    exp(optDebugInfo.preconditionedGradNorm),
    optDebugInfo.lineSearchStepSize],
    (name, value) => {
      return { name, value };
    }
  );

  const pathMapInfo = optDebugInfo.pathMap ? optDebugInfo.pathMap.map(([variable, value, grad, preconditionedGrad]) => ({
    variable,
    value: value.toFixed(3),
    grad: exp(grad),
    preconditionedGrad: exp(preconditionedGrad)
  })) : [];

  // TODO: hyperlink the shapes
  return (
    <div style={{ boxSizing: "border-box" }}>

      <DataTable
        data={optDebugInfoRows}
        title={"Status (evaluated between this frame and the previous)"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={[
          { name: "Name", selector: "name", sortable: true },
          { name: "Value", selector: "value", sortable: true },
        ]}
      />

      <DataTable
        data={pathMapInfo}
        title={"Varying variables"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={[
          { name: "Path", selector: "variable", sortable: true },
          { name: "Value", selector: "value", sortable: true },
          { name: "Grad", selector: "grad", sortable: true },
          { name: "Preconditioned grad", selector: "preconditionedGrad", sortable: true },
        ]}
      />

      < DataTable
        data={constrInfos}
        title={"Constraints"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={
          [
            { name: "Expression", selector: "name", sortable: true },
            { name: "Energy", selector: "energy", sortable: true },
            { name: "Satisfied?", selector: "sat", sortable: true },
          ]}
        conditionalRowStyles={
          [
            {
              when: (row) => row.sat === "no",
              style: {
                backgroundColor: `#ffcabe !important`,
              },
            },
          ]}
      />

      <DataTable
        data={objInfos}
        title={"Objectives"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={[
          { name: "Expression", selector: "name", sortable: true },
          { name: "Gradient Norm", selector: "gradientNorm", sortable: true },
        ]}
      />
    </div >
  );
};
export default Opt;
