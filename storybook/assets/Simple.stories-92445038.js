var i=Object.defineProperty;var t=(o,s)=>i(o,"name",{value:s,configurable:!0});import{S as r}from"./Simple-f13359ac.js";import{c as a,o as p,e as l}from"./PenrosePrograms-959e2bde.js";import{j as e}from"./jsx-runtime-a4b97114.js";import"./svg-cac72e8c.js";import"./iframe-57bf72f0.js";import"./vector-wedge.substance-4b9fff45.js";const x={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:r},n=t(o=>e("div",{style:{width:"50%",height:"50%"},children:e(r,{...o})}),"Template"),c=n.bind({});c.args=a;const m=n.bind({});m.args=p;const d=n.bind({});d.args=l;const C=["ContinuousMap","OneSet","Error"];export{c as ContinuousMap,d as Error,m as OneSet,C as __namedExportsOrder,x as default};
//# sourceMappingURL=Simple.stories-92445038.js.map