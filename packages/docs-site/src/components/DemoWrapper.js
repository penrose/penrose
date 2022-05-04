import BrowserOnly from "@docusaurus/BrowserOnly";
import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import React from "react";

// Hack bc penrose doesn't work in SSR??
let Demo;
if (ExecutionEnvironment.canUseDOM) {
  Demo = require("@penrose/components").Demo;
}

export default function DemoWrapper(props) {
  return (
    <BrowserOnly fallback={<div>Loading...</div>}>
      {() => <Demo {...props} />}
    </BrowserOnly>
  );
}
