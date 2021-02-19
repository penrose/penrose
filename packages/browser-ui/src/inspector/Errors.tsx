import { showError } from "@penrose/core";
import * as React from "react";
import IViewProps from "./views/IViewProps";
const Errors: React.FC<IViewProps> = ({ error }: IViewProps) => {
  if (!error) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no errors
      </div>
    );
  }
  return (
    <div style={{ padding: "1em" }}>
      <div style={{ fontWeight: 700 }}>1 error:</div>
      <pre>{showError(error).toString()}</pre>
    </div>
  );
};
export default Errors;
