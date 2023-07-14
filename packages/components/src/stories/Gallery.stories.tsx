import registry from "@penrose/examples/dist/registry.js";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import Gallery from "../Gallery";
const trios = [...registry.entries()]
  .filter(([, meta]) => meta.trio && meta.gallery)
  .map(([id]) => id);

export default {
  title: "Example/Gallery",
  component: Gallery,
} as ComponentMeta<typeof Gallery>;

const Template: ComponentStory<typeof Gallery> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Gallery trios={trios} ideLink="https://penrose.cs.cmu.edu/try/" />
  </div>
);

export const GalleryAll = Template.bind({});
