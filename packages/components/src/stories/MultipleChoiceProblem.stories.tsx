import substance from "@penrose/examples/dist/molecules/hydrogencyanide.substance";
import style from "@penrose/examples/dist/molecules/lewis.style";
import domain from "@penrose/examples/dist/molecules/molecules.domain";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import { ThemeProvider } from "styled-components";
import MultipleChoiceProblem from "../MultipleChoiceProblem.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Multiple Choice Problem Component",
  component: MultipleChoiceProblem,
} as ComponentMeta<typeof MultipleChoiceProblem>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof MultipleChoiceProblem> = (args) => (
  <ThemeProvider
    theme={{
      default: "#fff",
    }}
  >
    <MultipleChoiceProblem {...args} />
  </ThemeProvider>
);

export const Props = Template.bind({});
Props.args = {
  prompt: "Choose the correct Lewis structure for $\\mathrm{HCN}$.",
  diagrams: [
    {
      substance,
      domain,
      style,
      variation: "1",
      answer: true,
    },
    {
      substance,
      domain,
      style,
      variation: "2",
      answer: true,
    },
    {
      substance,
      domain,
      style,
      variation: "3",
      answer: false,
    },
    {
      substance,
      domain,
      style,
      variation: "4",
      answer: false,
    },
  ],
};
