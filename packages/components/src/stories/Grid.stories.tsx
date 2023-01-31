import { ThemeProvider } from "@material-ui/core";
import { repeat } from "@penrose/core/dist/utils/Util";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import { Grid } from "../Grid";
import penroseBlue from "../themes/penroseBlue";
import { continuousMap } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Grid Component",
  component: Grid,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Grid>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Grid> = (args) => (
  <div style={{ width: "50%", height: "50%" }}>
    <ThemeProvider theme={penroseBlue}>
      <Grid {...args} />
    </ThemeProvider>
  </div>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  style: continuousMap.style,
  domain: continuousMap.domain,
  substances: repeat(10, continuousMap.substance),
};

// export const OneSet = Template.bind({});
// OneSet.args = oneSet;

// export const Error = Template.bind({});
// Error.args = error;
