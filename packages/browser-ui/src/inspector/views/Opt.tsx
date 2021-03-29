import * as React from "react";
import IViewProps from "./IViewProps";
import { prettyPrintFn, evalFns } from "@penrose/core";
import { zip } from "lodash";
import DataTable from "react-data-table-component";
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
  ).map(([name, energy]) => ({ name, energy, type: "constraint" }));
  const objInfos = zip(
    frame.objFns.map(prettyPrintFn),
    evalFns(frame.objFns, frame)
  ).map(([name, energy]) => ({ name, energy, type: "objective" }));

  // TODO: hyperlink the shapes
  return (
    <div>
      <DataTable
        data={[...constrInfos, ...objInfos]}
        title={"Optimization"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={[
          { name: "Expression", sortable: true, selector: "name" },
          { name: "Energy", selector: "energy", sortable: true },
          { name: "Type", selector: "type", sortable: true }
        ]}
      />
    </div>
  );
};
export default Opt;
