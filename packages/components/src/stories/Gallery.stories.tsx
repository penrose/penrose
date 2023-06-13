import { ComponentMeta, ComponentStory } from "@storybook/react";
import Gallery from "../Gallery";

export default {
  title: "Example/Gallery",
  component: Gallery,
} as ComponentMeta<typeof Gallery>;

const Template: ComponentStory<typeof Gallery> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Gallery ideLink="https://penrose.cs.cmu.edu/try/" />
  </div>
);

export const GalleryAll = Template.bind({});
