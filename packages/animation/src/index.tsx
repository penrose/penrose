import React from "react";
import ReactDOM from "react-dom/client";
import { AnimationFrame } from "./AnimationFrame";

const root = ReactDOM.createRoot(
  document.getElementById("root") as HTMLElement,
);

root.render(
  <React.StrictMode>
    <AnimationFrame frameTitle={"animation"} />
  </React.StrictMode>,
);
