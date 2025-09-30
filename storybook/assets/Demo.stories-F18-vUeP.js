import{j as i}from"./jsx-runtime-QvZ8i92b.js";import{r as d}from"./index-uubelm5h.js";import{S as y,__tla as h}from"./Simple-DpOqLLOr.js";import{v as a}from"./PenrosePrograms-5n-t3br5.js";import{__tla as x}from"./svg-BcVDPuve.js";import{__tla as b}from"./vector-wedge.substance-Dr62AGZ5.js";import{__tla as f}from"./iframe-BHpLOgMN.js";import"../sb-preview/runtime.js";let e,u,c,R=Promise.all([(()=>{try{return h}catch{}})(),(()=>{try{return x}catch{}})(),(()=>{try{return b}catch{}})(),(()=>{try{return f}catch{}})()]).then(async()=>{var o,m,l;const s=t=>{const[p,g]=d.useState(0);d.useEffect(()=>{const v=window.setInterval(()=>g(_=>(_+1)%t.examples.length),5e3);return()=>clearInterval(v)},[p]);const r=t.examples[p];return i.jsx(y,{name:"demo",substance:r.sub,style:r.sty,domain:r.dsl,variation:r.variation,interactive:!1,animate:!0,stepSize:r.stepSize,imageResolver:r.imageResolver,excludeWarnings:[]})};s.__docgenInfo={description:"",methods:[],displayName:"Demo",props:{examples:{required:!0,tsType:{name:"Array",elements:[{name:"signature",type:"object",raw:`{
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
}[]`},description:""},darkMode:{required:!0,tsType:{name:"boolean"},description:""}}};let n;c={title:"Example/Demo Component",component:s},n=t=>i.jsx("div",{style:{width:"400px",height:"400px"},children:i.jsx(s,{...t})}),e=n.bind({}),e.args={examples:[{sub:a.substance,sty:a.style,dsl:a.domain,variation:a.variation}]},e.parameters={...e.parameters,docs:{...(o=e.parameters)==null?void 0:o.docs,source:{originalSource:`args => <div style={{
  width: "400px",
  height: "400px"
}}>
    <Demo {...args} />
  </div>`,...(l=(m=e.parameters)==null?void 0:m.docs)==null?void 0:l.source}}},u=["VectorExamples"]});export{e as VectorExamples,u as __namedExportsOrder,R as __tla,c as default};
