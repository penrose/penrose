import{r as a}from"./index.5c77362b.js";var s={exports:{}},n={};/**
 * @license React
 * react-jsx-runtime.production.min.js
 *
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */var i=a.exports,l=Symbol.for("react.element"),m=Symbol.for("react.fragment"),u=Object.prototype.hasOwnProperty,c=i.__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED.ReactCurrentOwner,y={key:!0,ref:!0,__self:!0,__source:!0};function x(t,r,_){var e,o={},p=null,f=null;_!==void 0&&(p=""+_),r.key!==void 0&&(p=""+r.key),r.ref!==void 0&&(f=r.ref);for(e in r)u.call(r,e)&&!y.hasOwnProperty(e)&&(o[e]=r[e]);if(t&&t.defaultProps)for(e in r=t.defaultProps,r)o[e]===void 0&&(o[e]=r[e]);return{$$typeof:l,type:t,key:p,ref:f,props:o,_owner:c.current}}n.Fragment=m;n.jsx=x;n.jsxs=x;s.exports=n;const d=s.exports.jsx,v=s.exports.jsxs,O=s.exports.Fragment;export{O as F,v as a,d as j};
//# sourceMappingURL=jsx-runtime.1c361ae2.js.map
