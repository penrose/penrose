import { evalFns, prettyPrintFn, zip2 } from "@penrose/core";
import DataTable from "react-data-table-component";
import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms.js";

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

  const { constraintSets, optStages, currentStageIndex, constrFns, objFns } =
    state;
  const { objMask, constrMask } = constraintSets.get(
    optStages[currentStageIndex]
  )!;

  const { constrEngs, objEngs } = evalFns(state);
  const constrEngsNamed = zip2(constrFns.map(prettyPrintFn), constrEngs);
  const constrInfos = zip2(constrEngsNamed, constrMask)
    .filter(([, include]) => include)
    .map((l) => l[0])
    .map(([name, fnEvaled]) => {
      const energy = parseFloat((Math.max(fnEvaled, 0)).toFixed(3));
      return {
        name,
        energy,
        sat: energy <= EPS ? "yes" : "no",
      };
    });

  // COMBAK: objective gradient norms are currently deprecated, because the
  // secondary outputs of the overall energy graph doesn't carry gradients of
  // intermediate nodes w.r.t. the inputs
  const objEngsNamed = zip2(objFns.map(prettyPrintFn), objEngs);
  const objInfos = zip2(objEngsNamed, objMask)
    .filter(([, include]) => include)
    .map((l) => l[0])
    .map(([name, fnEvaled]) => {
      const energy = parseFloat(fnEvaled.toFixed(3));
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
      <h2>Stage: {state.optStages[state.currentStageIndex]}</h2>
      <DataTable
        data={constrInfos}
        title={"Constraints"}
        dense={true}
        highlightOnHover={true}
        striped={true}
        columns={[
          { name: "Expression", selector: "name", sortable: true },
          { name: "Energy", selector: "energy", sortable: true, width: "7em" },
          { name: "Satisfied?", selector: "sat", sortable: true, width: "7em", fontWeight: "bold" },
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
          { name: "Expression", selector: "name", sortable: true  },
          { name: "Energy", selector: "energy", sortable: true, width: "10em" },
          // { name: "Gradient Norm", selector: "gradientNorm", sortable: true },
        ]}
      />
    </div>
  );
}
