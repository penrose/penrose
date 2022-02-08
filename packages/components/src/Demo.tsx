import { compileDomain } from "@penrose/core";
import { Simple } from ".";
import Listing from "./Listing";

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
          substanceString={props.sub}
          styleString={props.sty}
          domainString={props.dsl}
          initVariation={props.variation}
          interactive={false}
        />
      </div>
    </div>
  );
};

export default Demo;
