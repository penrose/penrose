import { Meta, StoryFn } from "@storybook/react";
import Embed from "../Embed.js";
import { continuousMap, error, oneSet } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Embed Component",
  component: Embed,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as Meta<typeof Embed>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: StoryFn<typeof Embed> = (args) => (
  <div style={{ width: "50%", height: "50%" }}>
    <Embed {...args} />
  </div>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = { trio: { ...continuousMap } };

export const OneSet = Template.bind({});
OneSet.args = { trio: { ...oneSet } };

export const Error = Template.bind({});
Error.args = {
  trio: {
    domain: error.domain,
    substance: "Set A + B",
    style: error.style,
    variation: "",
  },
};
