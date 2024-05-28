import{S as r,__tla as __tla_0}from"./Simple-f3e6e7d1.js";import{c as a,o as p,e as l}from"./PenrosePrograms-42ca5674.js";import{j as e}from"./jsx-runtime-f8704ad2.js";import{__tla as __tla_1}from"./svg-dfcfad54.js";import{__tla as __tla_2}from"./iframe-a9c88668.js";import{__tla as __tla_3}from"./vector-wedge.substance-fd4a2431.js";let c,d,m,C,x;let __tla=Promise.all([(()=>{try{return __tla_0}catch{}})(),(()=>{try{return __tla_1}catch{}})(),(()=>{try{return __tla_2}catch{}})(),(()=>{try{return __tla_3}catch{}})()]).then(async()=>{var i=Object.defineProperty;var t=(o,s)=>i(o,"name",{value:s,configurable:!0});let n;x={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:r};n=t(o=>e("div",{style:{width:"50%",height:"50%"},children:e(r,{...o})}),"Template");c=n.bind({});c.args=a;m=n.bind({});m.args=p;d=n.bind({});d.args=l;C=["ContinuousMap","OneSet","Error"]});export{c as ContinuousMap,d as Error,m as OneSet,C as __namedExportsOrder,x as default,__tla};