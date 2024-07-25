import{j as r}from"./jsx-runtime-1438501e.js";import{l as R,__tla as k}from"./svg-c8f61f75.js";import{s as a,F as P}from"./styled-components.browser.esm-60fac4ad.js";import{S as D,__tla as M}from"./Simple-add1c8a1.js";import{c as S}from"./PenrosePrograms-78669588.js";import"./index-f46741a2.js";import"./hoist-non-react-statics.cjs-84dc48a6.js";import{__tla as z}from"./vector-wedge.substance-92d42973.js";import{__tla as E}from"./iframe-9bbcd2e4.js";import"../sb-preview/runtime.js";let i,p,g,A=Promise.all([(()=>{try{return k}catch{}})(),(()=>{try{return M}catch{}})(),(()=>{try{return z}catch{}})(),(()=>{try{return E}catch{}})()]).then(async()=>{var d,l,c;const u=a.main`
  flex-grow: 1;
  margin-left: "4rem";
`,h=a.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`,y=a.div``,x=a.h3`
  font-family: "Roboto Mono";
  color: ${e=>e.theme.primary};
`,f=a.div`
  margin: 0.5rem;
  width: 25rem;
  height: 25rem;
  border-color: ${e=>e.theme.primary};
  border-width: 2px;
  border-style: solid;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
`,v=a.div`
  width: calc(100% - 0.75rem);
  height: 1.75rem;
  font-size: 1.25rem;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  padding: 0.5rem 0 0.5rem 0.75rem;
  vertical-align: text-bottom;
  color: ${e=>e.theme.primary};
`,_=a.div`
  color: ${e=>e.theme.primary};
  vertical-align: text-bottom;
  font-family: monospace;
`;function n(e){const{header:o,diagrams:b,imageResolver:j}=e,w=()=>b.map(({substance:q,domain:C,style:T,variation:G},t)=>r.jsxs(f,{children:[r.jsx(v,{children:r.jsx(_,{children:o(t)})}),r.jsx("div",{style:{height:"calc(100% - 2.75rem)",position:"relative"},children:r.jsx(D,{substance:q,variation:G,domain:C,style:T,name:`gridbox-${t}`,interactive:!1,imageResolver:j},`gridbox-${t}`)})]},`gridbox-container-${t}`)),$=e.diagrams.length===0?r.jsx(y,{children:r.jsx(x,{children:"(Generated diagrams will appear here)"})}):w();return r.jsx(u,{children:r.jsx(h,{children:$})})}n.__docgenInfo={description:"",methods:[],displayName:"Grid",props:{diagrams:{required:!0,tsType:{name:"Array",elements:[{name:"signature",type:"object",raw:`{
  style: string;
  domain: string;
  substance: string;
  variation: string;
}`,signature:{properties:[{key:"style",value:{name:"string",required:!0}},{key:"domain",value:{name:"string",required:!0}},{key:"substance",value:{name:"string",required:!0}},{key:"variation",value:{name:"string",required:!0}}]}}],raw:"DiagramSource[]"},description:""},header:{required:!0,tsType:{name:"signature",type:"function",raw:"(i: number) => string",signature:{arguments:[{type:{name:"number"},name:"i"}],return:{name:"string"}}},description:""},imageResolver:{required:!1,tsType:{name:"PathResolver"},description:""}}};let s,m;s={primary:"#40b4f7",secondary:"#C9C9C9"},g={title:"Example/Grid Component",component:n},m=e=>r.jsx(P,{theme:s,children:r.jsx(n,{...e,header:o=>`Diagram ${o}`})}),i=m.bind({}),i.args={diagrams:R.range(10).map(e=>({...S,variation:`${e}`}))},i.parameters={...i.parameters,docs:{...(d=i.parameters)==null?void 0:d.docs,source:{originalSource:"args => <ThemeProvider theme={penroseBlue}>\n    <Grid {...args} header={(i: number) => `Diagram ${i}`} />\n  </ThemeProvider>",...(c=(l=i.parameters)==null?void 0:l.docs)==null?void 0:c.source}}},p=["ContinuousMap"]});export{i as ContinuousMap,p as __namedExportsOrder,A as __tla,g as default};
