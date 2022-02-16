import { compileDomain } from "@penrose/core";
import * as React from "react";
import Listing from "./Listing";
import { Simple } from "./Simple";

const Demo = (props: {
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
  width: string; // the width of each half; total width is twice this
  // height must be equal to width (including in the passed Style canvas!)
}) => {
  const env = compileDomain(props.dsl).unsafelyUnwrap();
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
        value={props.sub}
        env={env}
        width={props.width}
        height={props.width}
      />
      <div style={{ width: props.width, height: props.width }}>
        <Simple
          substance={props.sub}
          style={props.sty}
          domain={props.dsl}
          variation={props.variation}
          interactive={false}
        />
      </div>
    </div>
  );
};

export default Demo;
