import { useMemo, useState } from "react";
import { DiagramPanel } from "./DiagramPanel.js";
import {
  AdaptiveGradientDescentOptimizer,
  ExteriorPointOptimizer,
  GradientDescentOptimizer,
  LBGFSOptimizer,
} from "./Optimizers.js";
import { TrioInfo, TrioSelector } from "./TrioSelector.js";

function App() {
  const [trioInfo, setTrioInfo] = useState<TrioInfo | null>(null);
  const [optimizer, setOptimizer] = useState<string>("gd");

  const gdOptimizer = useMemo(() => {
    console.log("new optimizer!");
    const gd = new GradientDescentOptimizer({
      stepSize: 1e-2,
      minGradient: 1e-2,
    });
    return new ExteriorPointOptimizer(gd);
  }, []);

  const adagradOptimizer = useMemo(() => {
    console.log("new adagrad optimizer!");
    const gd = new AdaptiveGradientDescentOptimizer({
      stepSize: 1e-2,
      minGradient: 1e-2,
    });
    return new ExteriorPointOptimizer(gd);
  }, []);

  const penroseOptimizer = useMemo(() => {
    console.log("new penrose optimizer!");
    return new LBGFSOptimizer();
  }, []);

  const optimizerObj = useMemo(() => {
    setTrioInfo((t) => (t ? { ...t } : null));

    switch (optimizer) {
      case "gd":
        return gdOptimizer;
      case "adagrad":
        return adagradOptimizer;
      case "penrose":
        return penroseOptimizer;
      default:
        throw new Error(`Unknown optimizer: ${optimizer}`);
    }
  }, [optimizer]);

  return (
    <>
      <TrioSelector setTrioInfo={setTrioInfo} />
      {/* Optimizer dropdown */}
      <select onChange={(e) => setOptimizer(e.target.value)}>
        <option value="gd">Gradient Descent</option>
        <option value="adagrad">Adagrad</option>
        <option value="penrose">Penrose</option>
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
