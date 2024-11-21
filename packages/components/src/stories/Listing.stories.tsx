import { Meta, StoryFn } from "@storybook/react";
import Listing from "../Listing.js";
import { continuousMap, oneSet } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Listing Component",
  component: Listing,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as Meta<typeof Listing>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: StoryFn<typeof Listing> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Listing {...args} />
  </div>
);

export const ContinuousMapDark = Template.bind({});
ContinuousMapDark.args = {
  domain: continuousMap.domain,
  src: continuousMap.substance,
  darkMode: true,
  width: "400px",
  height: "300px",
};

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  domain: continuousMap.domain,
  src: continuousMap.substance,
  width: "400px",
  height: "300px",
};

export const OneSet = Template.bind({});
OneSet.args = {
  domain: continuousMap.domain,
  src: oneSet.substance,
  width: "400px",
  height: "300px",
};
