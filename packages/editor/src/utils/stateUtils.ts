import { isPenroseError, runtimeError, showError } from "@penrose/core";
import { optimizer } from "../state/atoms";
import { OptimizerPromises } from "../worker/OptimizerWorker";

export const updateStateOnError = (
  error: any,
  setWorker: (valOrUpdater: any) => void,
  setDiagram: (valOrUpdater: any) => void,
) => {
  if (!isPenroseError(error)) {
    error = runtimeError(String(error));
  }
  console.error(showError(error));
  if (optimizer.getState() == "Error") {
    console.error("OptimizerWorker latching error: " + showError(error));
  }
  setDiagram((state: any) => ({
    ...state,
    warnings: [],
    error,
  }));
  setWorker((state: any) => ({
    ...state,
    compiling: false,
    optimizing: false,
  }));
};

export const handleOptimizerPromises = async (
  { onStart, onFinish }: OptimizerPromises,
  setWorker: (valOrSetter: any) => void,
  setDiagram: (valOrSetter: any) => void,
  onError?: (error: any) => void,
) => {
  const callerOnError = onError;
  onError = (error) => {
    updateStateOnError(error, setDiagram, setWorker);
    callerOnError?.(error);
  };

  try {
    onFinish
      .then((info) => {
        setDiagram((state: any) => ({
          ...state,
          state: info.state,
        }));
        setWorker((state: any) => ({
          ...state,
          optimizing: false,
        }));
      })
      .catch(onError);

    await onStart;
    setWorker((state: any) => ({
      ...state,
      optimizing: true,
    }));
  } catch (error: unknown) {
    onError(error);
  }
};
