import { ComponentMeta, ComponentStory } from "@storybook/react";
import TimelineTable from "../TimelineTable";

export default {
  title: "Example/Timeline Component",
  component: TimelineTable,
} as ComponentMeta<typeof TimelineTable>;

const Template: ComponentStory<typeof TimelineTable> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <TimelineTable {...args} />
  </div>
);

export const PenroseTimeline = Template.bind({});
PenroseTimeline.args = {
  events: [
    {
      start: new Date("2023-01-01"),
      end: new Date("2023-06-01"),
      task: "Web IDE",
      category: "outcome",
    },
    {
      start: new Date("2023-09-01"),
      end: new Date("2025-01-01"),
      task: "Integrations: Powerpoint, Canvas, LaTeX, Google Docs, etc.",
      category: "outcome",
    },

    {
      task: "Animation and Progressively-built diagrams",
      start: new Date("2023-12-01"),
      end: new Date("2024-12-01"),
      category: "outcome",
    },
    {
      task: "Standard Library that spans DOE Math Curriculum",
      start: new Date("2023-01-01"),
      end: new Date("2024-01-01"),
      category: "outcome",
    },
    {
      task: "Spatially localize alt text",
      start: new Date("2023-09-01"),
      end: new Date("2023-12-01"),
      category: "access",
    },
    {
      task: "Language localization",
      start: new Date("2023-06-01"),
      end: new Date("2023-09-01"),
      category: "access",
    },
    {
      task: "Color acuity optimization",
      start: new Date("2023-10-01"),
      end: new Date("2024-09-01"),
      category: "access",
    },
    {
      task: "Semantics-preserving, layout-optimized zoom",
      start: new Date("2024-01-01"),
      end: new Date("2025-06-01"),
      category: "access",
    },
    {
      task: "Improved compilation",
      start: new Date("2023-01-01"),
      end: new Date("2023-09-01"),
      category: "milestone",
    },
    {
      task: "Interactivity",
      start: new Date("2023-09-01"),
      end: new Date("2025-06-01"),
      category: "milestone",
    },
    {
      task: "Temporal specification",
      start: new Date("2023-09-01"),
      end: new Date("2024-01-01"),
      category: "milestone",
    },
    {
      task: "Enhanced graphical features",
      start: new Date("2023-01-02"),
      end: new Date("2025-06-01"),
      category: "milestone",
    },
    {
      task: "Staged optimization",
      start: new Date("2023-12-01"),
      end: new Date("2024-12-01"),
      category: "milestone",
    },
  ],
  categories: ["outcome", "access", "milestone"],
  start: new Date("2023-01-01"),
  end: new Date("2025-06-01"),
};
