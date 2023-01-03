var v=Object.defineProperty;var i=Object.getOwnPropertySymbols;var y=Object.prototype.hasOwnProperty,h=Object.prototype.propertyIsEnumerable;var d=(e,t,n)=>t in e?v(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,m=(e,t)=>{for(var n in t||(t={}))y.call(t,n)&&d(e,n,t[n]);if(i)for(var n of i(t))h.call(t,n)&&d(e,n,t[n]);return e};import{r as l}from"./index.5c77362b.js";import{L as x}from"./Listing.a8e18688.js";import{S as b}from"./Simple.bb195258.js";import{a as f,j as o}from"./jsx-runtime.1c361ae2.js";import{v as s,a}from"./PenrosePrograms.9c73e0e5.js";import"./index.33068e5f.js";const c=e=>{const[t,n]=l.exports.useState(0);l.exports.useEffect(()=>{const u=window.setInterval(()=>n(g=>(g+1)%e.examples.length),5e3);return()=>clearInterval(u)},[t]);const r=e.examples[t];return f("div",{style:{display:"flex",flexDirection:"row",height:"100%",flexWrap:"wrap"},children:[o(x,{domain:r.dsl,substance:r.sub,width:e.width,height:e.width,monacoOptions:{theme:e.darkMode?"vs-dark":"vs",wrappingIndent:"indent"}}),o("div",{style:{width:e.width,height:e.width},children:o(b,{substance:r.sub,style:r.sty,domain:r.dsl,variation:r.variation,interactive:!1,animate:!0})})]})};var p=c;c.__docgenInfo={description:"",methods:[],displayName:"Demo",props:{examples:{required:!0,tsType:{name:"Array",elements:[{name:"signature",type:"object",raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
}`,signature:{properties:[{key:"sub",value:{name:"string",required:!0}},{key:"sty",value:{name:"string",required:!0}},{key:"dsl",value:{name:"string",required:!0}},{key:"variation",value:{name:"string",required:!0}}]}}],raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
}[]`},description:""},width:{required:!0,tsType:{name:"string"},description:""},darkMode:{required:!0,tsType:{name:"boolean"},description:""}}};var C={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
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
`,locationsMap:{"vector-examples":{startLoc:{col:46,line:18},endLoc:{col:1,line:22},startBody:{col:46,line:18},endBody:{col:1,line:22}}}}},title:"Example/Demo Component",component:p};const w=e=>o("div",{style:{width:"100%",height:"100%"},children:o(p,m({},e))}),D=w.bind({});D.args={examples:[{sub:s.substance,sty:s.style,dsl:s.domain,variation:s.variation},{sub:a.substance,sty:a.style,dsl:a.domain,variation:a.variation}],width:"400px"};const W=["VectorExamples"];export{D as VectorExamples,W as __namedExportsOrder,C as default};
//# sourceMappingURL=Demo.stories.6bd4d5a6.js.map
