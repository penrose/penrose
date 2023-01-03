import{w as A,n as C,g as E,r as h,u as _,o as T}from"./vendor.e66cac25.js";var y="backgrounds",M;function L(o,a){return a||(a=o.slice(0)),Object.freeze(Object.defineProperties(o,{raw:{value:Object.freeze(a)}}))}var s=A.document,I=A.window,P=function(){var a=I.matchMedia("(prefers-reduced-motion: reduce)");return a.matches},R=function(a){var e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:[],r=arguments.length>2?arguments[2]:void 0;if(a==="transparent")return"transparent";if(e.find(function(t){return t.value===a}))return a;var n=e.find(function(t){return t.name===r});if(n)return n.value;if(r){var d=e.map(function(t){return t.name}).join(", ");C.warn(E(M||(M=L([`
        Backgrounds Addon: could not find the default color "`,`".
        These are the available colors for your story based on your configuration:
        `,`.
      `])),r,d))}return"transparent"},O=function(a){var e=Array.isArray(a)?a:[a];e.forEach($)},$=function(a){var e=s.getElementById(a);e&&e.parentElement.removeChild(e)},H=function(a,e){var r=s.getElementById(a);if(r)r.innerHTML!==e&&(r.innerHTML=e);else{var n=s.createElement("style");n.setAttribute("id",a),n.innerHTML=e,s.head.appendChild(n)}},N=function(a,e,r){var n=s.getElementById(a);if(n)n.innerHTML!==e&&(n.innerHTML=e);else{var d=s.createElement("style");d.setAttribute("id",a),d.innerHTML=e;var t="addon-backgrounds-grid".concat(r?"-docs-".concat(r):""),l=s.getElementById(t);l?l.parentElement.insertBefore(d,l):s.head.appendChild(d)}},G=function(a,e){var r,n=e.globals,d=e.parameters,t=(r=n[y])===null||r===void 0?void 0:r.value,l=d[y],i=h(function(){return l.disable?"transparent":R(t,l.values,l.default)},[l,t]),u=h(function(){return i&&i!=="transparent"},[i]),v=e.viewMode==="docs"?"#anchor--".concat(e.id," .docs-story"):".sb-show-main",g=h(function(){var f="transition: background-color 0.3s;";return`
      `.concat(v,` {
        background: `).concat(i,` !important;
        `).concat(P()?"":f,`
      }
    `)},[i,v]);return _(function(){var f=e.viewMode==="docs"?"addon-backgrounds-docs-".concat(e.id):"addon-backgrounds-color";if(!u){O(f);return}N(f,g,e.viewMode==="docs"?e.id:null)},[u,g,e]),a()},z;function D(o,a){return a||(a=o.slice(0)),Object.freeze(Object.defineProperties(o,{raw:{value:Object.freeze(a)}}))}var F=T(function(){},E(z||(z=D([`
    Backgrounds Addon: The cell size parameter has been changed.

    - parameters.grid.cellSize should now be parameters.backgrounds.grid.cellSize
    See https://github.com/storybookjs/storybook/blob/next/MIGRATION.md#deprecated-grid-parameter
  `])))),K=function(a,e){var r,n,d,t,l=e.globals,i=e.parameters,u=i[y].grid,v=((r=l[y])===null||r===void 0?void 0:r.grid)===!0&&u.disable!==!0,g=u.cellAmount,f=u.cellSize,p=u.opacity,S=e.viewMode==="docs",c;(n=i.grid)!==null&&n!==void 0&&n.cellSize?(c=i.grid.cellSize,F()):c=f;var j=i.layout===void 0||i.layout==="padded",w=j?16:0,m=(d=u.offsetX)!==null&&d!==void 0?d:S?20:w,b=(t=u.offsetY)!==null&&t!==void 0?t:S?20:w,B=h(function(){var k=e.viewMode==="docs"?"#anchor--".concat(e.id," .docs-story"):".sb-show-main",x=["".concat(c*g,"px ").concat(c*g,"px"),"".concat(c*g,"px ").concat(c*g,"px"),"".concat(c,"px ").concat(c,"px"),"".concat(c,"px ").concat(c,"px")].join(", ");return`
      `.concat(k,` {
        background-size: `).concat(x,` !important;
        background-position: `).concat(m,"px ").concat(b,"px, ").concat(m,"px ").concat(b,"px, ").concat(m,"px ").concat(b,"px, ").concat(m,"px ").concat(b,`px !important;
        background-blend-mode: difference !important;
        background-image: linear-gradient(rgba(130, 130, 130, `).concat(p,`) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, `).concat(p,`) 1px, transparent 1px),
         linear-gradient(rgba(130, 130, 130, `).concat(p/2,`) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, `).concat(p/2,`) 1px, transparent 1px) !important;
      }
    `)},[c]);return _(function(){var k=e.viewMode==="docs"?"addon-backgrounds-grid-docs-".concat(e.id):"addon-backgrounds-grid";if(!v){O(k);return}H(k,B)},[v,B,e]),a()},U=[K,G],X={backgrounds:{grid:{cellSize:20,opacity:.5,cellAmount:5},values:[{name:"light",value:"#F8F8F8"},{name:"dark",value:"#333333"}]}};export{U as decorators,X as parameters};
//# sourceMappingURL=preview.2bea19da.js.map
