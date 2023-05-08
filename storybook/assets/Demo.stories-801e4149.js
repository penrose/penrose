var u=Object.defineProperty;var a=(e,t)=>u(e,"name",{value:t,configurable:!0});import{r as m}from"./index-74c5fbfa.js";import{S as g}from"./Simple-baf96763.js";import{j as s}from"./jsx-runtime-9c5bc5e6.js";import{v as n,a as r}from"./PenrosePrograms-4dd03406.js";import"./es.object.get-own-property-descriptor-2f3bcc00.js";import"./web.url-1059d872.js";const i=a(e=>{const[t,c]=m.useState(0);m.useEffect(()=>{const d=window.setInterval(()=>c(p=>(p+1)%e.examples.length),5e3);return()=>clearInterval(d)},[t]);const o=e.examples[t];return s("div",{style:{width:e.width,height:e.width},children:s(g,{name:"demo",substance:o.sub,style:o.sty,domain:o.dsl,variation:o.variation,interactive:!1,animate:!0})})},"Demo"),l=i;try{i.displayName="Demo",i.__docgenInfo={description:"",displayName:"Demo",props:{examples:{defaultValue:null,description:"",name:"examples",required:!0,type:{name:"{ sub: string; sty: string; dsl: string; variation: string; }[]"}},width:{defaultValue:null,description:"",name:"width",required:!0,type:{name:"string"}},darkMode:{defaultValue:null,description:"",name:"darkMode",required:!0,type:{name:"boolean"}}}}}catch{}const E={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import Demo from "../Demo";
import { vectorsPerp, vectorWedge } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Demo Component",
  component: Demo,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Demo>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Demo> = (args) => (
  <div style={{ width: "100%", height: "100%" }}>
    <Demo {...args} />
  </div>
);

export const VectorExamples = Template.bind({});
VectorExamples.args = {
  examples: [
    {
      sub: vectorsPerp.substance,
      sty: vectorsPerp.style,
      dsl: vectorsPerp.domain,
      variation: vectorsPerp.variation,
    },
    {
      sub: vectorWedge.substance,
      sty: vectorWedge.style,
      dsl: vectorWedge.domain,
      variation: vectorWedge.variation,
    },
  ],
  width: "400px",
};
`,locationsMap:{"vector-examples":{startLoc:{col:46,line:18},endLoc:{col:1,line:22},startBody:{col:46,line:18},endBody:{col:1,line:22}}}}},title:"Example/Demo Component",component:l},y=a(e=>s("div",{style:{width:"100%",height:"100%"},children:s(l,{...e})}),"Template"),v=y.bind({});v.args={examples:[{sub:n.substance,sty:n.style,dsl:n.domain,variation:n.variation},{sub:r.substance,sty:r.style,dsl:r.domain,variation:r.variation}],width:"400px"};const M=["VectorExamples"];export{v as VectorExamples,M as __namedExportsOrder,E as default};
//# sourceMappingURL=Demo.stories-801e4149.js.map
