var s=Object.defineProperty;var n=(o,i)=>s(o,"name",{value:i,configurable:!0});import{S as r}from"./Simple-f86ba08a.js";import{c as p,o as a,e as l}from"./PenrosePrograms-31e6ae0d.js";import{j as e}from"./jsx-runtime-50ff9bfa.js";import"./svg-6cd73a06.js";import"./iframe-8d1c39a4.js";import"./CollectLabels-be2bb010.js";import"./vector-wedge.substance-0e092761.js";import"./resolver-b9429209.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-8995dd1a.js";import"./setTheory.domain-681bfd26.js";import"./venn.style-bb2a7220.js";const B={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:r},t=n(o=>e("div",{style:{width:"50%",height:"50%"},children:e(r,{...o})}),"Template"),m=t.bind({});m.args=p;const c=t.bind({});c.args=a;const d=t.bind({});d.args=l;const L=["ContinuousMap","OneSet","Error"];export{m as ContinuousMap,d as Error,c as OneSet,L as __namedExportsOrder,B as default};
//# sourceMappingURL=Simple.stories-f83743f7.js.map
