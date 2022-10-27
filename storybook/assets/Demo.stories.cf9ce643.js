var v=Object.defineProperty;var d=Object.getOwnPropertySymbols;var y=Object.prototype.hasOwnProperty,x=Object.prototype.propertyIsEnumerable;var m=(e,t,o)=>t in e?v(e,t,{enumerable:!0,configurable:!0,writable:!0,value:o}):e[t]=o,l=(e,t)=>{for(var o in t||(t={}))y.call(t,o)&&m(e,o,t[o]);if(d)for(var o of d(t))x.call(t,o)&&m(e,o,t[o]);return e};import{r as c}from"./index.b8d87d4b.js";import{L as h}from"./Listing.371b1921.js";import{S as f}from"./Simple.960745d3.js";import{a as b,j as r}from"./jsx-runtime.ae1cdb03.js";import{v as s,a}from"./PenrosePrograms.5806bc64.js";import"./index.33068e5f.js";const i=e=>{const[t,o]=c.exports.useState(0);c.exports.useEffect(()=>{const u=window.setInterval(()=>o(g=>(g+1)%e.examples.length),5e3);return()=>clearInterval(u)},[t]);const n=e.examples[t];return b("div",{style:{display:"flex",flexDirection:"row",height:"100%",flexWrap:"wrap"},children:[r(h,{domain:n.dsl,substance:n.sub,width:e.width,height:e.width,monacoOptions:{theme:e.darkMode?"vs-dark":"vs",wrappingIndent:"indent"}}),r("div",{style:{width:e.width,height:e.width},children:r(f,{substance:n.sub,style:n.sty,domain:n.dsl,variation:n.variation,interactive:!1,animate:!0})})]})};var p=i;try{i.displayName="Demo",i.__docgenInfo={description:"",displayName:"Demo",props:{examples:{defaultValue:null,description:"",name:"examples",required:!0,type:{name:"{ sub: string; sty: string; dsl: string; variation: string; }[]"}},width:{defaultValue:null,description:"",name:"width",required:!0,type:{name:"string"}},darkMode:{defaultValue:null,description:"",name:"darkMode",required:!0,type:{name:"boolean"}}}},typeof STORYBOOK_REACT_CLASSES!="undefined"&&(STORYBOOK_REACT_CLASSES["src/Demo.tsx#Demo"]={docgenInfo:i.__docgenInfo,name:"Demo",path:"src/Demo.tsx#Demo"})}catch{}var O={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"vector-examples":{startLoc:{col:46,line:18},endLoc:{col:1,line:22},startBody:{col:46,line:18},endBody:{col:1,line:22}}}}},title:"Example/Demo Component",component:p};const w=e=>r("div",{style:{width:"100%",height:"100%"},children:r(p,l({},e))}),D=w.bind({});D.args={examples:[{sub:s.substance,sty:s.style,dsl:s.domain,variation:s.variation},{sub:a.substance,sty:a.style,dsl:a.domain,variation:a.variation}],width:"400px"};const P=["VectorExamples"];export{D as VectorExamples,P as __namedExportsOrder,O as default};
//# sourceMappingURL=Demo.stories.cf9ce643.js.map
