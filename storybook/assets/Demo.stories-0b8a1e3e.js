import{j as s}from"./jsx-runtime-1438501e.js";import{r as u}from"./index-f46741a2.js";import{S as h,__tla as x}from"./Simple-532f144b.js";import{v as a}from"./PenrosePrograms-6a3ff97a.js";import{__tla as b}from"./svg-ad0265f3.js";import{__tla as f}from"./vector-wedge.substance-b36889c2.js";import{__tla as q}from"./iframe-dc105e68.js";import"../sb-preview/runtime.js";let e,c,g,S=Promise.all([(()=>{try{return x}catch{}})(),(()=>{try{return b}catch{}})(),(()=>{try{return f}catch{}})(),(()=>{try{return q}catch{}})()]).then(async()=>{var m,l,p;const i=t=>{const[d,v]=u.useState(0);u.useEffect(()=>{const _=window.setInterval(()=>v(y=>(y+1)%t.examples.length),5e3);return()=>clearInterval(_)},[d]);const r=t.examples[d];return s.jsx(h,{name:"demo",substance:r.sub,style:r.sty,domain:r.dsl,variation:r.variation,interactive:!1,animate:!0,stepSize:r.stepSize,imageResolver:r.imageResolver,excludeWarnings:[]})},n=i;i.__docgenInfo={description:"",methods:[],displayName:"Demo",props:{examples:{required:!0,tsType:{name:"Array",elements:[{name:"signature",type:"object",raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
  stepSize?: number;
  imageResolver?: PathResolver;
}`,signature:{properties:[{key:"sub",value:{name:"string",required:!0}},{key:"sty",value:{name:"string",required:!0}},{key:"dsl",value:{name:"string",required:!0}},{key:"variation",value:{name:"string",required:!0}},{key:"stepSize",value:{name:"number",required:!1}},{key:"imageResolver",value:{name:"PathResolver",required:!1}}]}}],raw:`{
  sub: string;
  sty: string;
  dsl: string;
  variation: string;
  stepSize?: number;
  imageResolver?: PathResolver;
}[]`},description:""},darkMode:{required:!0,tsType:{name:"boolean"},description:""}}};let o;g={title:"Example/Demo Component",component:n},o=t=>s.jsx("div",{style:{width:"400px",height:"400px"},children:s.jsx(n,{...t})}),e=o.bind({}),e.args={examples:[{sub:a.substance,sty:a.style,dsl:a.domain,variation:a.variation}]},e.parameters={...e.parameters,docs:{...(m=e.parameters)==null?void 0:m.docs,source:{originalSource:`args => <div style={{
  width: "400px",
  height: "400px"
}}>
    <Demo {...args} />
  </div>`,...(p=(l=e.parameters)==null?void 0:l.docs)==null?void 0:p.source}}},c=["VectorExamples"]});export{e as VectorExamples,c as __namedExportsOrder,S as __tla,g as default};
