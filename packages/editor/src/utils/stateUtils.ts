import { OptimizerPromises } from "../worker/OptimizerWorker";

export const handleOptimizerPromises = async (
  { onStart, onFinish }: OptimizerPromises,
  setWorker: (valOrSetter: any) => void,
  setDiagram: (valOrSetter: any) => void,
) => {
  onFinish.then((info) => {
    setDiagram((state: any) => ({
      ...state,
      state: info.state,
    }));
    setWorker((state: any) => ({
      ...state,
      optimizing: false,
    }));
  });

  await onStart;
  setWorker((state: any) => ({
    ...state,
    optimizing: true,
  }));
};
