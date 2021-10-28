import React from "react";
import logo from "./logo.svg";
import "./App.css";
import Embed from "./Embed";

const App = () => {
  return (
    <div style={{ margin: "0 auto", width: "50%", height: "50%" }}>
      <Embed
        substanceString={`
        AutoLabel All
        Set A`}
        styleString={`
        canvas {
          width = 800
          height = 700
        }
        Set X { X.shape = Circle {} }`}
        domainString={"type Set"}
      />
    </div>
  );
};

export default App;
