import { useEffect, useState } from "react";
import Listing from "./Listing";
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
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        height: "100%",
        flex: 1,
      }}
    >
      <Listing
        domain={example.dsl}
        substance={example.sub}
        width={props.width}
        height={props.width}
      />
      <div style={{ width: props.width, height: props.width }}>
        <Simple
          substance={example.sub}
          style={example.sty}
          domain={example.dsl}
          variation={example.variation}
          interactive={false}
          animate={true}
        />
      </div>
    </div>
  );
};

export default Demo;
