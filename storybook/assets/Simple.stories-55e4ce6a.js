var i=Object.defineProperty;var n=(o,s)=>i(o,"name",{value:s,configurable:!0});import{S as r}from"./Simple-ed96aa12.js";import{c as p,o as a,e as l}from"./PenrosePrograms-3c6b78f9.js";import{j as e}from"./jsx-runtime-1f455694.js";import"./svg-1cacda10.js";import"./iframe-97ab85e4.js";import"./CollectLabels-74366f83.js";import"./vector-wedge.substance-459b0b4e.js";import"./resolver-6e28dbec.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-dbff41d3.js";const E={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:r},t=n(o=>e("div",{style:{width:"50%",height:"50%"},children:e(r,{...o})}),"Template"),c=t.bind({});c.args=p;const m=t.bind({});m.args=a;const d=t.bind({});d.args=l;const T=["ContinuousMap","OneSet","Error"];export{c as ContinuousMap,d as Error,m as OneSet,T as __namedExportsOrder,E as default};
//# sourceMappingURL=Simple.stories-55e4ce6a.js.map
