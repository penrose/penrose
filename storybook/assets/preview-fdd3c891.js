var T=Object.defineProperty;var l=(t,r)=>T(t,"name",{value:r,configurable:!0});import{w as E,l as L}from"./es.object.get-own-property-descriptor-2f3bcc00.js";import"./web.url-1059d872.js";import{d as _,f as y,u as O,b as I}from"./iframe-e1cc6c70.js";import"./es.number.is-integer-dd6f716e.js";var S="backgrounds",z;function P(t,r){return r||(r=t.slice(0)),Object.freeze(Object.defineProperties(t,{raw:{value:Object.freeze(r)}}))}l(P,"_taggedTemplateLiteral$1");var g=E.document,R=E.window,$=l(function(){var r=R.matchMedia("(prefers-reduced-motion: reduce)");return r.matches},"isReduceMotionEnabled"),H=l(function(r){var e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:[],a=arguments.length>2?arguments[2]:void 0;if(r==="transparent")return"transparent";if(e.find(function(o){return o.value===r}))return r;var n=e.find(function(o){return o.name===a});if(n)return n.value;if(a){var d=e.map(function(o){return o.name}).join(", ");L.warn(_(z||(z=P([`
        Backgrounds Addon: could not find the default color "`,`".
        These are the available colors for your story based on your configuration:
        `,`.
      `])),a,d))}return"transparent"},"getBackgroundColorByName"),j=l(function(r){var e=Array.isArray(r)?r:[r];e.forEach(N)},"clearStyles"),N=l(function(r){var e=g.getElementById(r);e&&e.parentElement.removeChild(e)},"clearStyle"),G=l(function(r,e){var a=g.getElementById(r);if(a)a.innerHTML!==e&&(a.innerHTML=e);else{var n=g.createElement("style");n.setAttribute("id",r),n.innerHTML=e,g.head.appendChild(n)}},"addGridStyle"),D=l(function(r,e,a){var n=g.getElementById(r);if(n)n.innerHTML!==e&&(n.innerHTML=e);else{var d=g.createElement("style");d.setAttribute("id",r),d.innerHTML=e;var o="addon-backgrounds-grid".concat(a?"-docs-".concat(a):""),u=g.getElementById(o);u?u.parentElement.insertBefore(d,u):g.head.appendChild(d)}},"addBackgroundStyle"),F=l(function(r,e){var a,n=e.globals,d=e.parameters,o=(a=n[S])===null||a===void 0?void 0:a.value,u=d[S],i=y(function(){return u.disable?"transparent":H(o,u.values,u.default)},[u,o]),s=y(function(){return i&&i!=="transparent"},[i]),p=e.viewMode==="docs"?"#anchor--".concat(e.id," .docs-story"):".sb-show-main",f=y(function(){var v="transition: background-color 0.3s;";return`
      `.concat(p,` {
        background: `).concat(i,` !important;
        `).concat($()?"":v,`
      }
    `)},[i,p]);return O(function(){var v=e.viewMode==="docs"?"addon-backgrounds-docs-".concat(e.id):"addon-backgrounds-color";if(!s){j(v);return}D(v,f,e.viewMode==="docs"?e.id:null)},[s,f,e]),r()},"withBackground"),A;function K(t,r){return r||(r=t.slice(0)),Object.freeze(Object.defineProperties(t,{raw:{value:Object.freeze(r)}}))}l(K,"_taggedTemplateLiteral");var Y=I(function(){},_(A||(A=K([`
    Backgrounds Addon: The cell size parameter has been changed.

    - parameters.grid.cellSize should now be parameters.backgrounds.grid.cellSize
    See https://github.com/storybookjs/storybook/blob/next/MIGRATION.md#deprecated-grid-parameter
  `])))),U=l(function(r,e){var a,n,d,o,u=e.globals,i=e.parameters,s=i[S].grid,p=((a=u[S])===null||a===void 0?void 0:a.grid)===!0&&s.disable!==!0,f=s.cellAmount,v=s.cellSize,m=s.opacity,w=e.viewMode==="docs",c;(n=i.grid)!==null&&n!==void 0&&n.cellSize?(c=i.grid.cellSize,Y()):c=v;var x=i.layout===void 0||i.layout==="padded",B=x?16:0,b=(d=s.offsetX)!==null&&d!==void 0?d:w?20:B,k=(o=s.offsetY)!==null&&o!==void 0?o:w?20:B,M=y(function(){var h=e.viewMode==="docs"?"#anchor--".concat(e.id," .docs-story"):".sb-show-main",C=["".concat(c*f,"px ").concat(c*f,"px"),"".concat(c*f,"px ").concat(c*f,"px"),"".concat(c,"px ").concat(c,"px"),"".concat(c,"px ").concat(c,"px")].join(", ");return`
      `.concat(h,` {
        background-size: `).concat(C,` !important;
        background-position: `).concat(b,"px ").concat(k,"px, ").concat(b,"px ").concat(k,"px, ").concat(b,"px ").concat(k,"px, ").concat(b,"px ").concat(k,`px !important;
        background-blend-mode: difference !important;
        background-image: linear-gradient(rgba(130, 130, 130, `).concat(m,`) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, `).concat(m,`) 1px, transparent 1px),
         linear-gradient(rgba(130, 130, 130, `).concat(m/2,`) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, `).concat(m/2,`) 1px, transparent 1px) !important;
      }
    `)},[c]);return O(function(){var h=e.viewMode==="docs"?"addon-backgrounds-grid-docs-".concat(e.id):"addon-backgrounds-grid";if(!p){j(h);return}G(h,M)},[p,M,e]),r()},"withGrid"),Z=[U,F],V={backgrounds:{grid:{cellSize:20,opacity:.5,cellAmount:5},values:[{name:"light",value:"#F8F8F8"},{name:"dark",value:"#333333"}]}};export{Z as decorators,V as parameters};
//# sourceMappingURL=preview-fdd3c891.js.map
