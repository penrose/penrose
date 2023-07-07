import vector from "@penrose/examples/dist/exterior-algebra/vector-wedge.trio";
import geometry from "@penrose/examples/dist/geometry-domain/textbook_problems/c05p13.trio";
import laplace from "@penrose/examples/dist/walk-on-spheres/laplace-estimator.trio.js";

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

export const WalkOnSphere = Template.bind({});
WalkOnSphere.args = {
  trio: {
    substance: laplace.substance,
    style: laplace.style[0].contents,
    domain: laplace.domain,
    variation: "test3",
  },
  imageResolver: laplace.style[0].resolver,
};
export const Geometry = Template.bind({});
Geometry.args = {
  trio: {
    substance: geometry.substance,
    style: geometry.style[0].contents,
    domain: geometry.domain,
    variation: "test",
  },
  imageResolver: geometry.style[0].resolver,
};
export const Vector = Template.bind({});
Vector.args = {
  trio: {
    substance: vector.substance,
    style: vector.style[0].contents,
    domain: vector.domain,
    variation: "test",
  },
  imageResolver: vector.style[0].resolver,
};
