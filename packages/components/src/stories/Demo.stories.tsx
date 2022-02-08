import { compileDomain } from "@penrose/core";
import { ComponentStory, ComponentMeta } from "@storybook/react";

import Demo from "../Demo";
import { continuousMap, oneSet } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Demo Component",
  component: Demo,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Demo>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Demo> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Demo {...args} />
  </div>
);

export const ContinuousMap = Template.bind({});

ContinuousMap.args = {
  sub: continuousMap.substanceString,
  sty: continuousMap.styleString,
  dsl: continuousMap.domainString,
  variation: continuousMap.initVariation,
};

export const OneSet = Template.bind({});
OneSet.args = {
  sub: oneSet.substanceString,
  sty: oneSet.styleString,
  dsl: oneSet.domainString,
  variation: oneSet.initVariation,
};
