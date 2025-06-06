import { useMemo, useState } from "react";
import {
  BasicStagedOptimizer,
  ExteriorPointOptimizer,
  LBGFSOptimizer, LineSearchGDOptimizer, MultiStartStagedOptimizer, StagedOptimizer
} from "../Optimizers.js";
import { DiagramPanel } from "./DiagramPanel.js";
import { TrioInfo, TrioSelector } from "./TrioSelector.js";

function App() {
  const [trioInfo, setTrioInfo] = useState<TrioInfo | null>(null);
  const [optimizer, setOptimizer] = useState<string>("line search gd");

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

  const optimizerObj: StagedOptimizer = useMemo(() => {
    setTrioInfo((t) => (t ? { ...t } : null));

    switch (optimizer) {
      case "line search gd":
        return lineSearchGDOptimizer;
      case "penrose":
        return penroseOptimizer;
      case "multi-penrose":
        return multiStartPenroseOptimizer;
      default:
        throw new Error(`Unknown optimizer: ${optimizer}`);
    }
  }, [optimizer]);

  return (
    <>
      <TrioSelector setTrioInfo={setTrioInfo} />
      {/* Optimizer dropdown */}
      <select onChange={(e) => setOptimizer(e.target.value)}>
        <option value="line search gd">Line Search Gradient Descent</option>
        <option value="penrose">Penrose</option>
        <option value="multi-penrose">Multi-start Penrose</option>
        {/* Add more optimizers here as needed */}
      </select>

      {trioInfo && (
        <div
          style={{
            display: "grid",
            marginTop: "2em",
          }}
        >
          <DiagramPanel trioInfo={trioInfo} optimizer={optimizerObj} />
        </div>
      )}
    </>
  );
}

export default App;
