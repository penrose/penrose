var i=Object.defineProperty;var n=(o,s)=>i(o,"name",{value:s,configurable:!0});import{S as r}from"./Simple-f5c4006a.js";import{c as p,o as a,e as l}from"./PenrosePrograms-fcb11dfd.js";import{j as e}from"./jsx-runtime-c6652fc8.js";import"./svg-4a436c2c.js";import"./iframe-fa2344d7.js";import"./vector-wedge.substance-8426bbe1.js";import"./resolver-6b1c30e6.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-a0e1415b.js";const j={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { Simple } from "../Simple.js";
import { continuousMap, error, oneSet } from "./PenrosePrograms.js";

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
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:r},t=n(o=>e("div",{style:{width:"50%",height:"50%"},children:e(r,{...o})}),"Template"),c=t.bind({});c.args=p;const m=t.bind({});m.args=a;const d=t.bind({});d.args=l;const E=["ContinuousMap","OneSet","Error"];export{c as ContinuousMap,d as Error,m as OneSet,E as __namedExportsOrder,j as default};
//# sourceMappingURL=Simple.stories-6a5c2c74.js.map
