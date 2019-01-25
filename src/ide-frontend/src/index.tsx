import * as React from "react";
import * as ReactDOM from "react-dom";
import App from "./App";
import "./index.css";
import "@reach/menu-button/styles.css";
// import registerServiceWorker from "./registerServiceWorker";

ReactDOM.render(<App />, document.getElementById("root") as HTMLElement);

if (process.env.NODE_ENV !== "production") {
  localStorage.setItem("debug", "ide:*,renderer:*");
}

// registerServiceWorker();
