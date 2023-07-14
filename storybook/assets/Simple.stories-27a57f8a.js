var s=Object.defineProperty;var n=(o,i)=>s(o,"name",{value:i,configurable:!0});import{S as r}from"./Simple-a3368a88.js";import{c as p,o as a,e as l}from"./PenrosePrograms-917e3bf0.js";import{j as e}from"./jsx-runtime-c6f22766.js";import"./svg-2c756005.js";import"./iframe-f92d5283.js";import"./CollectLabels-8b207d01.js";import"./vector-wedge.substance-ec9995da.js";import"./resolver-7739be03.js";import"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import"./functions.domain-3369ef8f.js";import"./setTheory.domain-681bfd26.js";import"./venn.style-2183a54c.js";const B={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
//# sourceMappingURL=Simple.stories-27a57f8a.js.map
