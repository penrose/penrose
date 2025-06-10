import { useEffect, useMemo, useState } from "react";
import {
  BasicStagedOptimizer,
  ExteriorPointOptimizer,
  LBGFSOptimizer, LineSearchGDOptimizer, MultiStartStagedOptimizer, SimulatedAnnealing, StagedOptimizer
} from "../Optimizers.js";
import { DiagramPanel } from "./DiagramPanel.js";
import { TrioInfo, TrioSelector } from "./TrioSelector.js";

function App() {
  const [trioInfo, setTrioInfo] = useState<TrioInfo | null>(null);
  const [optimizer, setOptimizer] = useState<StagedOptimizer | null>(null);

  const lineSearchGDOptimizer = useMemo(() => {
    console.log("new line search gd optimizer!");
    const gd = new LineSearchGDOptimizer();
    return new BasicStagedOptimizer(new ExteriorPointOptimizer(gd));
  }, []);

  const penroseOptimizer = useMemo(() => {
    console.log("new penrose optimizer!");
    return new BasicStagedOptimizer(new LBGFSOptimizer());
  }, []);

  const multiStartPenroseOptimizer = useMemo(() => {
    console.log("new multi-penrose optimizer!");
    return new MultiStartStagedOptimizer(
      () => new ExteriorPointOptimizer(new LBGFSOptimizer()),
      16);
  }, []);

  const saLineSearchGDOptimizer = useMemo(() => {
    console.log("new SA line search gd optimizer!");
    const lbfgs = new LBGFSOptimizer();
    const simulatedAnnealing = new SimulatedAnnealing(lbfgs);
    const staged = new BasicStagedOptimizer(simulatedAnnealing);
    return staged;
  }, []);

  const strToOptimizer = (str: string): StagedOptimizer => {
    switch (str) {
      case "line search gd":
        return lineSearchGDOptimizer;
      case "penrose":
        return penroseOptimizer;
      case "multi-penrose":
        return multiStartPenroseOptimizer;
      case "SA lbfgs":
        return saLineSearchGDOptimizer;
      default:
        throw new Error(`Unknown optimizer: ${str}`);
    }
  };

  useEffect(() => {
    setOptimizer(strToOptimizer("line search gd"));
  }, []);

  console.log(trioInfo)
  console.log(optimizer);

  return (
    <>
      <TrioSelector setTrioInfo={setTrioInfo} />
      {/* Optimizer dropdown */}
      <select onChange={(e) => setOptimizer(strToOptimizer(e.target.value))}>
        <option value="line search gd">Line Search Gradient Descent</option>
        <option value="penrose">Penrose</option>
        <option value="multi-penrose">Multi-start Penrose</option>
        <option value="SA lbfgs">Simulated Annealing L-BFGS</option>
        {/* Add more optimizers here as needed */}
      </select>

      {trioInfo && optimizer &&
        <div
          style={{
            display: "grid",
            marginTop: "2em",
          }}
        >
          <DiagramPanel trioInfo={trioInfo} optimizer={optimizer} />
        </div>
      }
    </>
  );
}

export default App;
