import substance from "@penrose/examples/dist/walk-on-spheres/Laplace.substance";
import svg from "@penrose/examples/dist/walk-on-spheres/ball-shading.svg";
import domain from "@penrose/examples/dist/walk-on-spheres/walk-on-spheres.domain";
import style from "@penrose/examples/dist/walk-on-spheres/walk-on-spheres.style";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import StagedDiagram from "../StagedDiagram";

export default {
  title: "Example/StagedDiagram Component",
  component: StagedDiagram,
} as ComponentMeta<typeof StagedDiagram>;

const Template: ComponentStory<typeof StagedDiagram> = (args) => (
  <div style={{ width: "50%" }}>
    <StagedDiagram {...args} />
  </div>
);

export const PenroseTimeline = Template.bind({});
PenroseTimeline.args = {
  substance,
  style,
  domain,
  variation: "test",
  imageResolver: async () => svg,
};
