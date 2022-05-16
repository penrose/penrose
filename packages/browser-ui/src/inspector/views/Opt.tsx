import { evalFns, normList, prettyPrintFn } from "@penrose/core";
import { zipWith } from "lodash";
import * as React from "react";
import DataTable from "react-data-table-component";
import ViewProps from "./ViewProps";

export const EPS = 10e-3;

const Opt: React.FC<ViewProps> = ({ frame /*, history*/ }: ViewProps) => {
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
      const gradientNorm = normList(fnEvaled.gradf);
      return {
        name,
        gradientNorm,
      };
    }
  );

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
          { name: "Gradient Norm", selector: "gradientNorm", sortable: true },
        ]}
      />
    </div>
  );
};
export default Opt;
