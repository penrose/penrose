var d=Object.defineProperty;var i=Object.getOwnPropertySymbols;var u=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable;var a=(t,n,o)=>n in t?d(t,n,{enumerable:!0,configurable:!0,writable:!0,value:o}):t[n]=o,r=(t,n)=>{for(var o in n||(n={}))u.call(n,o)&&a(t,o,n[o]);if(i)for(var o of i(n))m.call(n,o)&&a(t,o,n[o]);return t};import{L as c}from"./Listing.aa546054.js";import{h as e,o as l}from"./PenrosePrograms.3f387182.js";import{j as p}from"./jsx-runtime.ae1cdb03.js";import"./index.b8d87d4b.js";import"./index.33068e5f.js";var k={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import Listing from "../Listing";
import { continuousMap, oneSet } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Listing Component",
  component: Listing,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Listing>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Listing> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Listing {...args} />
  </div>
);

export const ContinuousMapDark = Template.bind({});
ContinuousMapDark.args = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  width: "400px",
  height: "300px",
  monacoOptions: {
    theme: "vs-dark",
  },
};

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  domain: continuousMap.domain,
  substance: continuousMap.substance,
  width: "400px",
  height: "300px",
};

export const OneSet = Template.bind({});
OneSet.args = {
  domain: continuousMap.domain,
  substance: oneSet.substance,
  width: "400px",
  height: "300px",
};
`,locationsMap:{"continuous-map-dark":{startLoc:{col:49,line:18},endLoc:{col:1,line:22},startBody:{col:49,line:18},endBody:{col:1,line:22}},"continuous-map":{startLoc:{col:49,line:18},endLoc:{col:1,line:22},startBody:{col:49,line:18},endBody:{col:1,line:22}},"one-set":{startLoc:{col:49,line:18},endLoc:{col:1,line:22},startBody:{col:49,line:18},endBody:{col:1,line:22}}}}},title:"Example/Listing Component",component:c};const s=t=>p("div",{style:{width:"100%",height:"100%"},children:p(c,r({},t))}),g=s.bind({});g.args={domain:e.domain,substance:e.substance,width:"400px",height:"300px",monacoOptions:{theme:"vs-dark"}};const h=s.bind({});h.args={domain:e.domain,substance:e.substance,width:"400px",height:"300px"};const b=s.bind({});b.args={domain:e.domain,substance:l.substance,width:"400px",height:"300px"};const w=["ContinuousMapDark","ContinuousMap","OneSet"];export{h as ContinuousMap,g as ContinuousMapDark,b as OneSet,w as __namedExportsOrder,k as default};
//# sourceMappingURL=Listing.stories.580ccc78.js.map
