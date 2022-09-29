var l=Object.defineProperty;var r=Object.getOwnPropertySymbols;var c=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable;var s=(t,o,n)=>o in t?l(t,o,{enumerable:!0,configurable:!0,writable:!0,value:n}):t[o]=n,i=(t,o)=>{for(var n in o||(o={}))c.call(o,n)&&s(t,n,o[n]);if(r)for(var n of r(o))m.call(o,n)&&s(t,n,o[n]);return t};import{S as p}from"./Simple.3029c536.js";import{h as d,o as u,i as g}from"./PenrosePrograms.b9c01514.js";import{j as a}from"./jsx-runtime.ae1cdb03.js";import"./index.b8d87d4b.js";var E={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"continuous-map":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}},error:{startLoc:{col:48,line:18},endLoc:{col:1,line:22},startBody:{col:48,line:18},endBody:{col:1,line:22}}}}},title:"Example/Simple Component",component:p};const e=t=>a("div",{style:{width:"50%",height:"50%"},children:a(p,i({},t))}),S=e.bind({});S.args=d;const y=e.bind({});y.args=u;const M=e.bind({});M.args=g;const T=["ContinuousMap","OneSet","Error"];export{S as ContinuousMap,M as Error,y as OneSet,T as __namedExportsOrder,E as default};
//# sourceMappingURL=Simple.stories.3b7d8614.js.map
