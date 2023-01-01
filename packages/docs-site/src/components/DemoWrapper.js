import BrowserOnly from "@docusaurus/BrowserOnly";
import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import { useColorMode } from "@docusaurus/theme-common";

// Hack bc penrose doesn't work in SSR??
let Demo;
if (ExecutionEnvironment.canUseDOM) {
  Demo = require("@penrose/components").Demo;
}

export default function DemoWrapper(props) {
  const { isDarkTheme } = useColorMode();
  return (
    <BrowserOnly fallback={<div>Loading...</div>}>
      {() => <Demo {...props} darkMode={isDarkTheme} />}
    </BrowserOnly>
  );
}
