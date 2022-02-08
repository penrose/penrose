import { compileDomain } from "@penrose/core";
import { Simple } from ".";
import Listing from "./Listing";

const Demo = (props: {
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
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
      <Listing value={props.sub} env={env} />
      <Simple
        substanceString={props.sub}
        styleString={props.sty}
        domainString={props.dsl}
        initVariation={props.variation}
        interactive={false}
      />
    </div>
  );
};

export default Demo;
