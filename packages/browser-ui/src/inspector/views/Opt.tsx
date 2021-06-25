import * as React from "react";
import IViewProps from "./IViewProps";
import { prettyPrintFn, evalFns } from "@penrose/core";
import { zip, sum } from "lodash";
import DataTable from "react-data-table-component";

export const EPS = 10e-3;

const vnorm = (v: number[]): number => {
  return Math.sqrt(sum(v.map((e) => e * e)));
};

const Opt: React.FC<IViewProps> = ({ frame, history }: IViewProps) => {
  if (!frame) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no frame
      </div>
    );
  }
  const constrInfos = zip(
    frame.constrFns.map(prettyPrintFn),
    evalFns(frame.constrFns, frame)
  ).map(([name, fnEvaled]) => {
    const energy = Math.max(fnEvaled?.f!, 0);
    return {
      name,
      energy,
      sat: energy <= EPS ? "yes" : "no",
    };
  });
  const objInfos = zip(
    frame.objFns.map(prettyPrintFn),
    evalFns(frame.objFns, frame)
  ).map(([name, fnEvaled]) => {
    const gradientNorm = vnorm(fnEvaled?.gradf!);
    return {
      name,
      gradientNorm,
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
              backgroundColor: `rgb(255, 202, 190) !important`,
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
