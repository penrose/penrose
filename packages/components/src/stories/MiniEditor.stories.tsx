import mesh2d from "@penrose/examples/dist/triangle-mesh-2d";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import MiniEditor from "../editing/MiniEditor";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/MiniEditor Component",
  component: MiniEditor,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof MiniEditor>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof MiniEditor> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <MiniEditor {...args} />
  </div>
);

export const Mesh = Template.bind({});
Mesh.args = {
  style: mesh2d["triangle-mesh-2d.style"],
  domain: mesh2d["triangle-mesh-2d.domain"],
};
