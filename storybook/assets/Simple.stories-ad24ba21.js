var i=Object.defineProperty;var n=(o,s)=>i(o,"name",{value:s,configurable:!0});import{S as r}from"./Simple-2c01fb3e.js";import{c as p,o as a,e as l}from"./PenrosePrograms-1f2fa0da.js";import{j as e}from"./jsx-runtime-74202484.js";import"./svg-e2a9632f.js";import"./iframe-916e8a48.js";import"./CollectLabels-35011149.js";import"./vector-wedge.substance-ed38d9fe.js";import"./resolver-8e6b3de6.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-21a98008.js";const E={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
//# sourceMappingURL=Simple.stories-ad24ba21.js.map
