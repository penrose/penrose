var T=Object.defineProperty;var l=(t,a)=>T(t,"name",{value:a,configurable:!0});import{n as E,l as L,d as x,x as y,u as O,k as I}from"./iframe-196f87c1.js";var S="backgrounds",z;function P(t,a){return a||(a=t.slice(0)),Object.freeze(Object.defineProperties(t,{raw:{value:Object.freeze(a)}}))}l(P,"_taggedTemplateLiteral$1");var g=E.document,R=E.window,$=l(function(){var a=R.matchMedia("(prefers-reduced-motion: reduce)");return a.matches},"isReduceMotionEnabled"),H=l(function(a){var e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:[],r=arguments.length>2?arguments[2]:void 0;if(a==="transparent")return"transparent";if(e.find(function(o){return o.value===a}))return a;var n=e.find(function(o){return o.name===r});if(n)return n.value;if(r){var d=e.map(function(o){return o.name}).join(", ");L.warn(x(z||(z=P([`
        Backgrounds Addon: could not find the default color "`,`".
        These are the available colors for your story based on your configuration:
        `,`.
      `])),r,d))}return"transparent"},"getBackgroundColorByName"),_=l(function(a){var e=Array.isArray(a)?a:[a];e.forEach(N)},"clearStyles"),N=l(function(a){var e=g.getElementById(a);e&&e.parentElement.removeChild(e)},"clearStyle"),G=l(function(a,e){var r=g.getElementById(a);if(r)r.innerHTML!==e&&(r.innerHTML=e);else{var n=g.createElement("style");n.setAttribute("id",a),n.innerHTML=e,g.head.appendChild(n)}},"addGridStyle"),D=l(function(a,e,r){var n=g.getElementById(a);if(n)n.innerHTML!==e&&(n.innerHTML=e);else{var d=g.createElement("style");d.setAttribute("id",a),d.innerHTML=e;var o="addon-backgrounds-grid".concat(r?"-docs-".concat(r):""),u=g.getElementById(o);u?u.parentElement.insertBefore(d,u):g.head.appendChild(d)}},"addBackgroundStyle"),F=l(function(a,e){var r,n=e.globals,d=e.parameters,o=(r=n[S])===null||r===void 0?void 0:r.value,u=d[S],i=y(function(){return u.disable?"transparent":H(o,u.values,u.default)},[u,o]),s=y(function(){return i&&i!=="transparent"},[i]),p=e.viewMode==="docs"?"#anchor--".concat(e.id," .docs-story"):".sb-show-main",f=y(function(){var v="transition: background-color 0.3s;";return`
      `.concat(p,` {
        background: `).concat(i,` !important;
        `).concat($()?"":v,`
      }
    `)},[i,p]);return O(function(){var v=e.viewMode==="docs"?"addon-backgrounds-docs-".concat(e.id):"addon-backgrounds-color";if(!s){_(v);return}D(v,f,e.viewMode==="docs"?e.id:null)},[s,f,e]),a()},"withBackground"),A;function K(t,a){return a||(a=t.slice(0)),Object.freeze(Object.defineProperties(t,{raw:{value:Object.freeze(a)}}))}l(K,"_taggedTemplateLiteral");var Y=I(function(){},x(A||(A=K([`
    Backgrounds Addon: The cell size parameter has been changed.

    - parameters.grid.cellSize should now be parameters.backgrounds.grid.cellSize
    See https://github.com/storybookjs/storybook/blob/next/MIGRATION.md#deprecated-grid-parameter
  `])))),U=l(function(a,e){var r,n,d,o,u=e.globals,i=e.parameters,s=i[S].grid,p=((r=u[S])===null||r===void 0?void 0:r.grid)===!0&&s.disable!==!0,f=s.cellAmount,v=s.cellSize,m=s.opacity,B=e.viewMode==="docs",c;(n=i.grid)!==null&&n!==void 0&&n.cellSize?(c=i.grid.cellSize,Y()):c=v;var j=i.layout===void 0||i.layout==="padded",M=j?16:0,b=(d=s.offsetX)!==null&&d!==void 0?d:B?20:M,k=(o=s.offsetY)!==null&&o!==void 0?o:B?20:M,w=y(function(){var h=e.viewMode==="docs"?"#anchor--".concat(e.id," .docs-story"):".sb-show-main",C=["".concat(c*f,"px ").concat(c*f,"px"),"".concat(c*f,"px ").concat(c*f,"px"),"".concat(c,"px ").concat(c,"px"),"".concat(c,"px ").concat(c,"px")].join(", ");return`
      `.concat(h,` {
        background-size: `).concat(C,` !important;
        background-position: `).concat(b,"px ").concat(k,"px, ").concat(b,"px ").concat(k,"px, ").concat(b,"px ").concat(k,"px, ").concat(b,"px ").concat(k,`px !important;
        background-blend-mode: difference !important;
        background-image: linear-gradient(rgba(130, 130, 130, `).concat(m,`) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, `).concat(m,`) 1px, transparent 1px),
         linear-gradient(rgba(130, 130, 130, `).concat(m/2,`) 1px, transparent 1px),
         linear-gradient(90deg, rgba(130, 130, 130, `).concat(m/2,`) 1px, transparent 1px) !important;
      }
    `)},[c]);return O(function(){var h=e.viewMode==="docs"?"addon-backgrounds-grid-docs-".concat(e.id):"addon-backgrounds-grid";if(!p){_(h);return}G(h,w)},[p,w,e]),a()},"withGrid"),q=[U,F],J={backgrounds:{grid:{cellSize:20,opacity:.5,cellAmount:5},values:[{name:"light",value:"#F8F8F8"},{name:"dark",value:"#333333"}]}};export{q as decorators,J as parameters};
//# sourceMappingURL=preview-4361c74c.js.map
