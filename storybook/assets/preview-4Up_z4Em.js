const{global:i}=__STORYBOOK_MODULE_GLOBAL__,{addons:s}=__STORYBOOK_MODULE_PREVIEW_API__,{STORY_CHANGED:h}=__STORYBOOK_MODULE_CORE_EVENTS__;var O="storybook/highlight",r="storybookHighlight",E=`${O}/add`,g=`${O}/reset`,{document:n}=i,m=(e="#FF4785",t="dashed")=>`
  outline: 2px ${t} ${e};
  outline-offset: 2px;
  box-shadow: 0 0 0 6px rgba(255,255,255,0.6);
`,l=s.getChannel(),p=e=>{let t=r;d();let o=Array.from(new Set(e.elements)),_=n.createElement("style");_.setAttribute("id",t),_.innerHTML=o.map(a=>`${a}{
          ${m(e.color,e.style)}
         }`).join(" "),n.head.appendChild(_)},d=()=>{var o;let e=r,t=n.getElementById(e);t&&((o=t.parentNode)==null||o.removeChild(t))};l.on(h,d),l.on(g,d),l.on(E,p);
