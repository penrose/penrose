var m=Object.defineProperty;var i=(r,e)=>m(r,"name",{value:e,configurable:!0});import{l,z as c}from"./PenrosePrograms-ee7c95b2.js";import{s as a,G as u,F as g,p as h}from"./penroseBlue-939b320b.js";import{R as f,r as y}from"./index-74c5fbfa.js";import{j as t}from"./jsx-runtime-9c5bc5e6.js";import"./es.object.get-own-property-descriptor-2f3bcc00.js";import"./web.url-1059d872.js";import"./index-b79ff3a9.js";import"./Simple-21ae824a.js";const x=a.main`
  flex-grow: 1;
  margin-left: "4rem";
`,S=a.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`,G=a.div``,b=a.h3`
  font-family: "Roboto Mono";
  color: ${r=>r.theme.primary};
`;class n extends f.Component{constructor(e){super(e)}setSrcState=e=>{this.setState({srcState:e})};innerContent(){return this.props.diagrams.map(({substance:e,domain:s,style:d,variation:p},o)=>y.createElement(u,{...this.props.gridBoxProps,key:`grid-${o}`,name:`grid-${o}`,header:this.props.header(o),metadata:this.props.metadata(o),domain:s,style:d,gridIndex:o,substance:e,variation:p,onSelected:this.props.onSelected,onStateUpdate:this.props.onStateUpdate,imageResolver:this.props.imageResolver}))}render(){const e=this.props.diagrams.length===0?t(G,{children:t(b,{children:"(Generated diagrams will appear here)"})}):this.innerContent();return t(x,{children:t(S,{children:e})})}}i(n,"Grid");try{n.displayName="Grid",n.__docgenInfo={description:"",displayName:"Grid",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramSource[]"}},metadata:{defaultValue:null,description:"",name:"metadata",required:!0,type:{name:"(i: number) => { name: string; data: string; }[]"}},header:{defaultValue:null,description:"",name:"header",required:!0,type:{name:"(i: number) => string"}},onSelected:{defaultValue:null,description:"",name:"onSelected",required:!1,type:{name:"((n: number) => void)"}},onStateUpdate:{defaultValue:null,description:"",name:"onStateUpdate",required:!0,type:{name:"(n: number, s: State) => void"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},gridBoxProps:{defaultValue:null,description:"",name:"gridBoxProps",required:!1,type:{name:"Partial<GridboxProps>"}}}}}catch{}const q={parameters:{storySource:{source:`import { ComponentMeta, ComponentStory } from "@storybook/react";
import { range } from "lodash";
import { ThemeProvider } from "styled-components";
import { Grid } from "../Grid";
import penroseBlue from "../themes/penroseBlue";
import { continuousMap } from "./PenrosePrograms";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Grid Component",
  component: Grid,
  // More on argTypes: https://storybook.js.org/docs/react/api/argtypes
  // argTypes: {
  //   backgroundColor: { control: 'color' },
  // },
} as ComponentMeta<typeof Grid>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Grid> = (args) => (
  <ThemeProvider theme={penroseBlue}>
    <Grid
      {...args}
      header={(i) => \`Diagram \${i}\`}
      metadata={(i) => [
        { name: "Variation", data: args.diagrams[i].variation },
      ]}
    />
  </ThemeProvider>
);

export const ContinuousMap = Template.bind({});
ContinuousMap.args = {
  diagrams: range(10).map((n) => ({
    ...continuousMap,
    variation: \`\${n}\`,
  })),
};

// export const OneSet = Template.bind({});
// OneSet.args = oneSet;

// export const Error = Template.bind({});
// Error.args = error;
`,locationsMap:{"continuous-map":{startLoc:{col:46,line:21},endLoc:{col:1,line:31},startBody:{col:46,line:21},endBody:{col:1,line:31}}}}},title:"Example/Grid Component",component:n},v=i(r=>t(g,{theme:h,children:t(n,{...r,header:e=>`Diagram ${e}`,metadata:e=>[{name:"Variation",data:r.diagrams[e].variation}]})}),"Template"),C=v.bind({});C.args={diagrams:l.range(10).map(r=>({...c,variation:`${r}`}))};const $=["ContinuousMap"];export{C as ContinuousMap,$ as __namedExportsOrder,q as default};
//# sourceMappingURL=Grid.stories-d1fc7b54.js.map
