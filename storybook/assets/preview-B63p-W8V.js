import{d as E}from"./index-DrFu-skq.js";const{useMemo:b,useEffect:_}=__STORYBOOK_MODULE_PREVIEW_API__,{global:M}=__STORYBOOK_MODULE_GLOBAL__,{logger:h}=__STORYBOOK_MODULE_CLIENT_LOGGER__;var c="backgrounds",{document:l,window:k}=M,O=()=>k.matchMedia("(prefers-reduced-motion: reduce)").matches,L=(a,e=[],r)=>{if(a==="transparent")return"transparent";if(e.find(n=>n.value===a))return a;let t=e.find(n=>n.name===r);if(t)return t.value;if(r){let n=e.map(o=>o.name).join(", ");h.warn(E`
        Backgrounds Addon: could not find the default color "${r}".
        These are the available colors for your story based on your configuration:
        ${n}.
      `)}return"transparent"},v=a=>{(Array.isArray(a)?a:[a]).forEach(w)},w=a=>{var r;let e=l.getElementById(a);e&&((r=e.parentElement)==null||r.removeChild(e))},B=(a,e)=>{let r=l.getElementById(a);if(r)r.innerHTML!==e&&(r.innerHTML=e);else{let t=l.createElement("style");t.setAttribute("id",a),t.innerHTML=e,l.head.appendChild(t)}},T=(a,e,r)=>{var n;let t=l.getElementById(a);if(t)t.innerHTML!==e&&(t.innerHTML=e);else{let o=l.createElement("style");o.setAttribute("id",a),o.innerHTML=e;let i=`addon-backgrounds-grid${r?`-docs-${r}`:""}`,d=l.getElementById(i);d?(n=d.parentElement)==null||n.insertBefore(o,d):l.head.appendChild(o)}},A=(a,e)=>{var g;let{globals:r,parameters:t}=e,n=(g=r[c])==null?void 0:g.value,o=t[c],i=b(()=>o.disable?"transparent":L(n,o.values,o.default),[o,n]),d=b(()=>i&&i!=="transparent",[i]),s=e.viewMode==="docs"?`#anchor--${e.id} .docs-story`:".sb-show-main",u=b(()=>`
      ${s} {
        background: ${i} !important;
        ${O()?"":"transition: background-color 0.3s;"}
      }
    `,[i,s]);return _(()=>{let p=e.viewMode==="docs"?`addon-backgrounds-docs-${e.id}`:"addon-backgrounds-color";if(!d){v(p);return}T(p,u,e.viewMode==="docs"?e.id:null)},[d,u,e]),a()},I=(a,e)=>{var x;let{globals:r,parameters:t}=e,n=t[c].grid,o=((x=r[c])==null?void 0:x.grid)===!0&&n.disable!==!0,{cellAmount:i,cellSize:d,opacity:s}=n,u=e.viewMode==="docs",g=t.layout===void 0||t.layout==="padded"?16:0,p=n.offsetX??(u?20:g),m=n.offsetY??(u?20:g),f=b(()=>{let $=e.viewMode==="docs"?`#anchor--${e.id} .docs-story`:".sb-show-main",y=[`${d*i}px ${d*i}px`,`${d*i}px ${d*i}px`,`${d}px ${d}px`,`${d}px ${d}px`].join(", ");return`
      ${$} {
        background-size: ${y} !important;
        background-position: ${p}px ${m}px, ${p}px ${m}px, ${p}px ${m}px, ${p}px ${m}px !important;
        background-blend-mode: difference !important;
        background-image: linear-gradient(rgba(130, 130, 130, ${s}) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, ${s}) 1px, transparent 1px),
         linear-gradient(rgba(130, 130, 130, ${s/2}) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, ${s/2}) 1px, transparent 1px) !important;
      }
    `},[d]);return _(()=>{let $=e.viewMode==="docs"?`addon-backgrounds-grid-docs-${e.id}`:"addon-backgrounds-grid";if(!o){v($);return}B($,f)},[o,f,e]),a()},H=[I,A],R={[c]:{grid:{cellSize:20,opacity:.5,cellAmount:5},values:[{name:"light",value:"#F8F8F8"},{name:"dark",value:"#333333"}]}},S={[c]:null};export{H as decorators,S as globals,R as parameters};
