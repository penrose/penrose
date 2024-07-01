import { Meta, StoryFn } from "@storybook/react";
import { range } from "lodash";
import { ThemeProvider } from "styled-components";
import Grid from "../Grid.js";
import penroseBlue from "../themes/penroseBlue.js";
import { continuousMap } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Grid Component",
  component: Grid,
} as Meta<typeof Grid>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: StoryFn<typeof Grid> = (args) => (
  <ThemeProvider theme={penroseBlue}>
    <Grid {...args} header={(i: number) => `Diagram ${i}`} />
  </ThemeProvider>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  diagrams: range(10).map((n) => ({
    ...continuousMap,
    variation: `${n}`,
  })),
};
