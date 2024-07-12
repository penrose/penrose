import { Meta, StoryFn } from "@storybook/react";
import { Simple } from "../Simple.js";
import { continuousMap, error, oneSet } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Simple Component",
  component: Simple,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as Meta<typeof Simple>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: StoryFn<typeof Simple> = (args) => (
  <div style={{ width: "50%", height: "50%" }}>
    <Simple {...args} />
  </div>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = continuousMap;

export const OneSet = Template.bind({});
OneSet.args = oneSet;

export const Error = Template.bind({});
Error.args = error;
