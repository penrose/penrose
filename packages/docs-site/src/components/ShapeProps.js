import BrowserOnly from "@docusaurus/BrowserOnly";
import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import React from "react";

// Hack bc penrose doesn't work in headless??
let _ShapeProps;
if (ExecutionEnvironment.canUseDOM) {
  _ShapeProps = require("./_ShapeProps.js").default;
}

export default function ShapeProps({ shapeName }) {
  return (
    <BrowserOnly fallback={<div>Loading...</div>}>
      {() => <_ShapeProps shapeName={shapeName} />}
    </BrowserOnly>
  );
}
