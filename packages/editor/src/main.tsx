import React from "react";
import ReactDOM from "react-dom";
import "./index.css";
import "flexlayout-react/style/light.css";
import "react-toastify/dist/ReactToastify.css";
import Router from "./Router";

ReactDOM.render(
  <React.StrictMode>
    <Router />
  </React.StrictMode>,
  document.getElementById("root")
);
