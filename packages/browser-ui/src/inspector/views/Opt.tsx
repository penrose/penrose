import { evalFns } from "@penrose/core";
import * as React from "react";
import DataTable from "react-data-table-component";
import IViewProps from "./IViewProps";

export const EPS = 10e-3;

const Opt: React.FC<IViewProps> = ({ frame /*, history*/ }: IViewProps) => {
  if (!frame) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no frame
      </div>
    );
  }

  const { constrEngs, objEngs } = evalFns(frame);
  const constrInfos = [...constrEngs.entries()].map(([name, fnEvaled]) => {
    const energy = Math.max(fnEvaled, 0);
    return {
      name,
      energy,
      sat: energy <= EPS ? "yes" : "no",
    };
  });

  // COMBAK: objective gradient norms are currently deprecated, because the
  // secondary outputs of the overall energy graph doesn't carry gradients of
  // intermediate nodes w.r.t. the inputs
  const objInfos = [...objEngs.entries()].map(([name, fnEvaled]) => {
    const energy = fnEvaled;
    // const gradientNorm = normList(fnEvaled.gradf);
    return {
      name,
      energy,
      // gradientNorm,
    };
  });

  // TODO: hyperlink the shapes
  return (
    <div style={{ boxSizing: "border-box" }}>
      <DataTable
        data={constrInfos}
        title={"Constraints"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={[
          { name: "Expression", selector: "name", sortable: true },
          { name: "Energy", selector: "energy", sortable: true },
          { name: "Satisfied?", selector: "sat", sortable: true },
        ]}
        conditionalRowStyles={[
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
          { name: "Energy", selector: "energy", sortable: true },
          // { name: "Gradient Norm", selector: "gradientNorm", sortable: true },
        ]}
      />
    </div>
  );
};
export default Opt;
