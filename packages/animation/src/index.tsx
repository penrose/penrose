import React from "react";
import ReactDOM from "react-dom/client";
import { AnimationFrame } from "./AnimationFrame";
import App from "./App";

const root = ReactDOM.createRoot(
  document.getElementById("root") as HTMLElement,
);

root.render(
  <React.StrictMode>
    <App />
    <AnimationFrame frameTitle="frame 1" />
  </React.StrictMode>,
);
