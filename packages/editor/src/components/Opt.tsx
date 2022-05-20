import { evalFns } from "@penrose/core";
import DataTable from "react-data-table-component";
import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms";

export const EPS = 10e-3;

export default function Opt() {
  const { state } = useRecoilValue(diagramState);
  if (!state) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no frame
      </div>
    );
  }

  const { constrEngs, objEngs } = evalFns(state);
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
}
