import{S as t}from"./Simple.86aaa81d.js";import{g as r,o as s,h as i}from"./PenrosePrograms.bf5fe2a2.js";import{j as n}from"./jsx-runtime.1c361ae2.js";import"./index.5c77362b.js";var u={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { Simple } from "../Simple";
import { continuousMap, error, oneSet } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Simple Component",
  component: Simple,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Simple>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Simple> = (args) => (
  <div style={{ width: "50%", height: "50%" }}>
    <Simple {...args} />
  </div>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = continuousMap;

export const OneSet = Template.bind({});
OneSet.args = oneSet;

export const Error = Template.bind({});
Error.args = error;
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:t};const o=e=>n("div",{style:{width:"50%",height:"50%"},children:n(t,{...e})}),a=o.bind({});a.args=r;const p=o.bind({});p.args=s;const l=o.bind({});l.args=i;const S=["ContinuousMap","OneSet","Error"];export{a as ContinuousMap,l as Error,p as OneSet,S as __namedExportsOrder,u as default};
//# sourceMappingURL=Simple.stories.d6b64588.js.map
