import { useEffect, useMemo, useState } from "react";
import {
  AutoMALAOptimizer,
  BasicStagedOptimizer,
  ExteriorPointOptimizer,
  LBGFSOptimizer,
  LineSearchGDOptimizer,
  MALAOptimizer,
  MultiStartStagedOptimizer,
  StagedOptimizer,
} from "../Optimizers.js";
import { DiagramPanel } from "./DiagramPanel.js";
import { TrioInfo, TrioSelector } from "./TrioSelector.js";

function App() {
  const [trioInfo, setTrioInfo] = useState<TrioInfo | null>(null);
  const [optimizer, setOptimizer] = useState<StagedOptimizer | null>(null);

  const lineSearchGDOptimizer = useMemo(() => {
    const gd = new LineSearchGDOptimizer();
    return new BasicStagedOptimizer(new ExteriorPointOptimizer(gd));
  }, []);

  const lbfgsOptimizer = useMemo(() => {
    return new BasicStagedOptimizer(new LBGFSOptimizer());
  }, []);

  const multiStartLbfgsOptimizer = useMemo(() => {
    return new MultiStartStagedOptimizer(
      () => new ExteriorPointOptimizer(new LBGFSOptimizer()),
      16,
    );
  }, []);

  const malaOptimizer = useMemo(() => {
    const mala = new MALAOptimizer({
      initialTemperature: 100,
      coolingRate: 0.001,
      constraintWeight: 100,
      stepSize: 0.0001,
      minAcceptanceRate: 1e-5,
    });
    return new BasicStagedOptimizer(mala);
  }, []);

  const autoMalaOptimizer = useMemo(() => {
    const mala = new AutoMALAOptimizer({
      initTemperature: 1000,
      coolingRate: 0.05,
      constraintWeight: 100,
      initStepSize: 1.0,
      minTemperature: 0.1,
    });
    return new BasicStagedOptimizer(mala);
  }, []);

  const strToOptimizer = (str: string): StagedOptimizer => {
    switch (str) {
      case "line search gd":
        return lineSearchGDOptimizer;
      case "lbfgs":
        return lbfgsOptimizer;
      case "multi-lbfgs":
        return multiStartLbfgsOptimizer;
      case "mala":
        return malaOptimizer;
      case "auto-mala":
        return autoMalaOptimizer;
      default:
        throw new Error(`Unknown optimizer: ${str}`);
    }
  };

  useEffect(() => {
    setOptimizer(strToOptimizer("line search gd"));
  }, []);

  console.log(trioInfo);
  console.log(optimizer);

  return (
    <>
      <TrioSelector setTrioInfo={setTrioInfo} />
      {/* Optimizer dropdown */}
      <select onChange={(e) => setOptimizer(strToOptimizer(e.target.value))}>
        <option value="line search gd">Line Search GD</option>
        <option value="lbfgs">L-BFGS</option>
        <option value="multi-lbfgs">Multi-start L-BFGS</option>
        <option value="mala">Simulated Annealing (MALA)</option>
        <option value="auto-mala">Simulated Annealing (AutoMALA)</option>
        {/* Add more optimizers here as needed */}
      </select>

      {trioInfo && optimizer && (
        <div
          style={{
            display: "grid",
            marginTop: "2em",
          }}
        >
          <DiagramPanel trioInfo={trioInfo} optimizer={optimizer} />
        </div>
      )}
    </>
  );
}

export default App;
