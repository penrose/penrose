import{r as l}from"./index-f46741a2.js";var s={exports:{}},a={};/**
* @license React
* react-jsx-runtime.production.min.js
*
* Copyright (c) Facebook, Inc. and its affiliates.
*
* This source code is licensed under the MIT license found in the
* LICENSE file in the root directory of this source tree.
*/var y=l,i=Symbol.for("react.element"),u=Symbol.for("react.fragment"),v=Object.prototype.hasOwnProperty,c=y.__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED.ReactCurrentOwner,m={key:!0,ref:!0,__self:!0,__source:!0};function p(o,e,n){var r,t={},f=null,_=null;n!==void 0&&(f=""+n),e.key!==void 0&&(f=""+e.key),e.ref!==void 0&&(_=e.ref);for(r in e)v.call(e,r)&&!m.hasOwnProperty(r)&&(t[r]=e[r]);if(o&&o.defaultProps)for(r in e=o.defaultProps,e)t[r]===void 0&&(t[r]=e[r]);return{$$typeof:i,type:o,key:f,ref:_,props:t,_owner:c.current}}a.Fragment=u,a.jsx=p,a.jsxs=p,s.exports=a;var O=s.exports;export{O as j};
