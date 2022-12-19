import { ComponentMeta, ComponentStory } from "@storybook/react";
import Listing from "../Listing";
import { continuousMap, oneSet } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Listing Component",
  component: Listing,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Listing>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Listing> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Listing {...args} />
  </div>
);

export const ContinuousMapDark = Template.bind({});
ContinuousMapDark.args = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  width: "400px",
  height: "300px",
  monacoOptions: {
    theme: "vs-dark",
  },
};

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  width: "400px",
  height: "300px",
};

export const OneSet = Template.bind({});
OneSet.args = {
  domain: continuousMap.domain,
  substance: oneSet.substance,
  width: "400px",
  height: "300px",
};
