import { PathResolver } from "@penrose/core";
import { useEffect, useState } from "react";
import { Simple } from "./Simple.js";

const Demo = (props: {
  examples: {
    sub: string;
    sty: string;
    dsl: string;
    variation: string;
    stepSize?: number;
    imageResolver?: PathResolver;
  }[];
  darkMode: boolean;
}) => {
  const [index, setIndex] = useState(0);

  useEffect(() => {
    const interval = window.setInterval(
      () => setIndex((i) => (i + 1) % props.examples.length),
      5000,
    );
    return () => clearInterval(interval);
  }, [index]);

  const example = props.examples[index];

  return (
    <Simple
      name="demo"
      substance={example.sub}
      style={example.sty}
      domain={example.dsl}
      variation={example.variation}
      interactive={false}
      animate={true}
      stepSize={example.stepSize}
      imageResolver={example.imageResolver}
      excludeWarnings={[]}
    />
  );
};

export default Demo;
