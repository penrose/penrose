import { useEffect, useState } from "react";
import { Simple } from "./Simple";

const Demo = (props: {
  examples: {
    sub: string;
    sty: string;
    dsl: string;
    variation: string;
  }[];
  width: string; // the width of each half; total width is twice this
  // height must be equal to width (including in the passed Style canvas!)
  darkMode: boolean;
}) => {
  const [index, setIndex] = useState(0);

  useEffect(() => {
    const interval = window.setInterval(
      () => setIndex((i) => (i + 1) % props.examples.length),
      5000
    );
    return () => clearInterval(interval);
  }, [index]);

  const example = props.examples[index];

  return (
    <Simple
      substance={example.sub}
      style={example.sty}
      domain={example.dsl}
      variation={example.variation}
      interactive={false}
      animate={true}
    />
  );
};

export default Demo;
