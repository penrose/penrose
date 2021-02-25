import { showError } from "@penrose/core";
import * as React from "react";
import IViewProps from "./IViewProps";
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
      <div style={{ fontFamily: "monospace" }}>
        {showError(error)
          .toString()
          .split("\n")
          .map((line: string, key: number) => (
            <p key={`err-ln-${key}`} style={{ margin: 0 }}>
              {line}
            </p>
          ))}
      </div>
    </div>
  );
};
export default Errors;
