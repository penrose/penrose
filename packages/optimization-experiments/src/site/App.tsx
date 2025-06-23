import { useEffect, useMemo, useState } from "react";
import {
  AutoMALAOptimizer,
  BasicStagedOptimizer,
  ExteriorPointOptimizer,
  LBGFSOptimizer,
  LineSearchGDOptimizer,
  MALAOptimizer,
  MultiStartStagedOptimizer, SimulatedAnnealing,
  StagedOptimizer
} from "../Optimizers.js";
import { DiagramPanel } from "./DiagramPanel.js";
import { TrioInfo, TrioSelector } from "./TrioSelector.js";
import { AutoMALA } from "../samplers.js";

function App() {
  const [trioInfo, setTrioInfo] = useState<TrioInfo | null>(null);
  const [optimizer, setOptimizer] = useState<StagedOptimizer | null>(null);

  const lbfgs = useMemo(() => {
    return new BasicStagedOptimizer(new LBGFSOptimizer());
  }, []);

  const multiStartLbfgs = useMemo(() => {
    return new MultiStartStagedOptimizer(
      () => new ExteriorPointOptimizer(new LBGFSOptimizer()),
      16,
    );
  }, []);

  const autoMalaSA = useMemo(() => {
    // const mala = new AutoMALAOptimizer({
    //   initTemperature: 1000,
    //   coolingRate: 0.05,
    //   initStepSize: 1.0,
    //   constraintWeight: 1000,
    //   minTemperature: 0.1,
    //   maxStepSearches: 30,
    //   roundLength: 100,
    // });
    const autoMala = new AutoMALA({
      initStepSize: 1.0,
      roundLength: 100,
      constraintWeight: 1000,
      maxStepSearches: 30,
    });
    const sa = new SimulatedAnnealing(
      autoMala,
      {
        initTemperature: 1000,
        coolingRate: 0.01,
        minTemperature: 0.1,
      });
    return new BasicStagedOptimizer(sa);
  }, []);

  const strToOptimizer = (str: string): StagedOptimizer => {
    switch (str) {
      case "lbfgs":
        return lbfgs;
      case "multi-lbfgs":
        return multiStartLbfgs;
      case "sa-auto-mala":
        return autoMalaSA;
      default:
        throw new Error(`Unknown optimizer: ${str}`);
    }
  };

  useEffect(() => {
    setOptimizer(strToOptimizer("lbfgs"));
  }, []);

  console.log(trioInfo);
  console.log(optimizer);

  return (
    <>
      <TrioSelector setTrioInfo={setTrioInfo} />
      {/* Optimizer dropdown */}
      <select onChange={(e) => setOptimizer(strToOptimizer(e.target.value))}>
        <option value="lbfgs">L-BFGS</option>
        <option value="multi-lbfgs">Multi-start L-BFGS</option>
        <option value="sa-auto-mala">Simulated Annealing (AutoMALA)</option>
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
