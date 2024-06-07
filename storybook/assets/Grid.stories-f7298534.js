import{l as f,__tla as __tla_0}from"./svg-8f79f423.js";import{s as o,F as y}from"./styled-components.browser.esm-f9fd8fa0.js";import{S as x,__tla as __tla_1}from"./Simple-a57acd4b.js";import{j as r,a as b}from"./jsx-runtime-fa54a4ba.js";import{c as v}from"./PenrosePrograms-962f201d.js";import{__tla as __tla_2}from"./iframe-2edd0afa.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-09a3cb37.js";import{__tla as __tla_3}from"./vector-wedge.substance-0b9ca64f.js";let S,z,N;let __tla=Promise.all([(()=>{try{return __tla_0}catch{}})(),(()=>{try{return __tla_1}catch{}})(),(()=>{try{return __tla_2}catch{}})(),(()=>{try{return __tla_3}catch{}})()]).then(async()=>{var h=Object.defineProperty;var i=(e,t)=>h(e,"name",{value:t,configurable:!0});const C=o.main`
  flex-grow: 1;
  margin-left: "4rem";
`,G=o.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`,w=o.div``,M=o.h3`
  font-family: "Roboto Mono";
  color: ${e=>e.theme.primary};
`,_=o.div`
  margin: 0.5rem;
  width: 25rem;
  height: 25rem;
  border-color: ${e=>e.theme.primary};
  border-width: 2px;
  border-style: solid;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
`,$=o.div`
  width: calc(100% - 0.75rem);
  height: 1.75rem;
  font-size: 1.25rem;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  padding: 0.5rem 0 0.5rem 0.75rem;
  vertical-align: text-bottom;
  color: ${e=>e.theme.primary};
`,j=o.div`
  color: ${e=>e.theme.primary};
  vertical-align: text-bottom;
  font-family: monospace;
`;function a(e){const{header:t,diagrams:s,imageResolver:m}=e,d=i(()=>s.map(({substance:c,domain:p,style:u,variation:g},n)=>b(_,{children:[r($,{children:r(j,{children:t(n)})}),r("div",{style:{height:"calc(100% - 2.75rem)",position:"relative"},children:r(x,{substance:c,variation:g,domain:p,style:u,name:`gridbox-${n}`,interactive:!1,imageResolver:m},`gridbox-${n}`)})]},`gridbox-container-${n}`)),"innerContent"),l=e.diagrams.length===0?r(w,{children:r(M,{children:"(Generated diagrams will appear here)"})}):d();return r(C,{children:r(G,{children:l})})}i(a,"Grid");try{a.displayName="Grid",a.__docgenInfo={description:"",displayName:"Grid",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramSource[]"}},header:{defaultValue:null,description:"",name:"header",required:!0,type:{name:"(i: number) => string"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}}}}}catch{}let P,T;P={primary:"#40b4f7",secondary:"#C9C9C9"};N={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { range } from "lodash";
import { ThemeProvider } from "styled-components";
import Grid from "../Grid.js";
import penroseBlue from "../themes/penroseBlue.js";
import { continuousMap } from "./PenrosePrograms.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Grid Component",
  component: Grid,
} as ComponentMeta<typeof Grid>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Grid> = (args) => (
  <ThemeProvider theme={penroseBlue}>
    <Grid {...args} header={(i: number) => \`Diagram \${i}\`} />
  </ThemeProvider>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  diagrams: range(10).map((n) => ({
    ...continuousMap,
    variation: \`\${n}\`,
  })),
};
`,locationsMap:{"continuous-map":{startLoc:{col:46,line:17},endLoc:{col:1,line:21},startBody:{col:46,line:17},endBody:{col:1,line:21}}}}},title:"Example/Grid Component",component:a};T=i(e=>r(y,{theme:P,children:r(a,{...e,header:t=>`Diagram ${t}`})}),"Template");S=T.bind({});S.args={diagrams:f.range(10).map(e=>({...v,variation:`${e}`}))};z=["ContinuousMap"]});export{S as ContinuousMap,z as __namedExportsOrder,N as default,__tla};