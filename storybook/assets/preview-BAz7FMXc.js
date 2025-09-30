import{d as s}from"./index-DrFu-skq.js";const{useMemo:u,useEffect:m}=__STORYBOOK_MODULE_PREVIEW_API__,{global:i}=__STORYBOOK_MODULE_GLOBAL__;var d=t=>{(Array.isArray(t)?t:[t]).forEach(x)},x=t=>{let o=typeof t=="string"?t:t.join(""),n=i.document.getElementById(o);n&&n.parentElement&&n.parentElement.removeChild(n)},$=(t,o)=>{let n=i.document.getElementById(t);if(n)n.innerHTML!==o&&(n.innerHTML=o);else{let l=i.document.createElement("style");l.setAttribute("id",t),l.innerHTML=o,i.document.head.appendChild(l)}},r="outline";function c(t){return s`
    ${t} body {
      outline: 1px solid #2980b9 !important;
    }

    ${t} article {
      outline: 1px solid #3498db !important;
    }

    ${t} nav {
      outline: 1px solid #0088c3 !important;
    }

    ${t} aside {
      outline: 1px solid #33a0ce !important;
    }

    ${t} section {
      outline: 1px solid #66b8da !important;
    }

    ${t} header {
      outline: 1px solid #99cfe7 !important;
    }

    ${t} footer {
      outline: 1px solid #cce7f3 !important;
    }

    ${t} h1 {
      outline: 1px solid #162544 !important;
    }

    ${t} h2 {
      outline: 1px solid #314e6e !important;
    }

    ${t} h3 {
      outline: 1px solid #3e5e85 !important;
    }

    ${t} h4 {
      outline: 1px solid #449baf !important;
    }

    ${t} h5 {
      outline: 1px solid #c7d1cb !important;
    }

    ${t} h6 {
      outline: 1px solid #4371d0 !important;
    }

    ${t} main {
      outline: 1px solid #2f4f90 !important;
    }

    ${t} address {
      outline: 1px solid #1a2c51 !important;
    }

    ${t} div {
      outline: 1px solid #036cdb !important;
    }

    ${t} p {
      outline: 1px solid #ac050b !important;
    }

    ${t} hr {
      outline: 1px solid #ff063f !important;
    }

    ${t} pre {
      outline: 1px solid #850440 !important;
    }

    ${t} blockquote {
      outline: 1px solid #f1b8e7 !important;
    }

    ${t} ol {
      outline: 1px solid #ff050c !important;
    }

    ${t} ul {
      outline: 1px solid #d90416 !important;
    }

    ${t} li {
      outline: 1px solid #d90416 !important;
    }

    ${t} dl {
      outline: 1px solid #fd3427 !important;
    }

    ${t} dt {
      outline: 1px solid #ff0043 !important;
    }

    ${t} dd {
      outline: 1px solid #e80174 !important;
    }

    ${t} figure {
      outline: 1px solid #ff00bb !important;
    }

    ${t} figcaption {
      outline: 1px solid #bf0032 !important;
    }

    ${t} table {
      outline: 1px solid #00cc99 !important;
    }

    ${t} caption {
      outline: 1px solid #37ffc4 !important;
    }

    ${t} thead {
      outline: 1px solid #98daca !important;
    }

    ${t} tbody {
      outline: 1px solid #64a7a0 !important;
    }

    ${t} tfoot {
      outline: 1px solid #22746b !important;
    }

    ${t} tr {
      outline: 1px solid #86c0b2 !important;
    }

    ${t} th {
      outline: 1px solid #a1e7d6 !important;
    }

    ${t} td {
      outline: 1px solid #3f5a54 !important;
    }

    ${t} col {
      outline: 1px solid #6c9a8f !important;
    }

    ${t} colgroup {
      outline: 1px solid #6c9a9d !important;
    }

    ${t} button {
      outline: 1px solid #da8301 !important;
    }

    ${t} datalist {
      outline: 1px solid #c06000 !important;
    }

    ${t} fieldset {
      outline: 1px solid #d95100 !important;
    }

    ${t} form {
      outline: 1px solid #d23600 !important;
    }

    ${t} input {
      outline: 1px solid #fca600 !important;
    }

    ${t} keygen {
      outline: 1px solid #b31e00 !important;
    }

    ${t} label {
      outline: 1px solid #ee8900 !important;
    }

    ${t} legend {
      outline: 1px solid #de6d00 !important;
    }

    ${t} meter {
      outline: 1px solid #e8630c !important;
    }

    ${t} optgroup {
      outline: 1px solid #b33600 !important;
    }

    ${t} option {
      outline: 1px solid #ff8a00 !important;
    }

    ${t} output {
      outline: 1px solid #ff9619 !important;
    }

    ${t} progress {
      outline: 1px solid #e57c00 !important;
    }

    ${t} select {
      outline: 1px solid #e26e0f !important;
    }

    ${t} textarea {
      outline: 1px solid #cc5400 !important;
    }

    ${t} details {
      outline: 1px solid #33848f !important;
    }

    ${t} summary {
      outline: 1px solid #60a1a6 !important;
    }

    ${t} command {
      outline: 1px solid #438da1 !important;
    }

    ${t} menu {
      outline: 1px solid #449da6 !important;
    }

    ${t} del {
      outline: 1px solid #bf0000 !important;
    }

    ${t} ins {
      outline: 1px solid #400000 !important;
    }

    ${t} img {
      outline: 1px solid #22746b !important;
    }

    ${t} iframe {
      outline: 1px solid #64a7a0 !important;
    }

    ${t} embed {
      outline: 1px solid #98daca !important;
    }

    ${t} object {
      outline: 1px solid #00cc99 !important;
    }

    ${t} param {
      outline: 1px solid #37ffc4 !important;
    }

    ${t} video {
      outline: 1px solid #6ee866 !important;
    }

    ${t} audio {
      outline: 1px solid #027353 !important;
    }

    ${t} source {
      outline: 1px solid #012426 !important;
    }

    ${t} canvas {
      outline: 1px solid #a2f570 !important;
    }

    ${t} track {
      outline: 1px solid #59a600 !important;
    }

    ${t} map {
      outline: 1px solid #7be500 !important;
    }

    ${t} area {
      outline: 1px solid #305900 !important;
    }

    ${t} a {
      outline: 1px solid #ff62ab !important;
    }

    ${t} em {
      outline: 1px solid #800b41 !important;
    }

    ${t} strong {
      outline: 1px solid #ff1583 !important;
    }

    ${t} i {
      outline: 1px solid #803156 !important;
    }

    ${t} b {
      outline: 1px solid #cc1169 !important;
    }

    ${t} u {
      outline: 1px solid #ff0430 !important;
    }

    ${t} s {
      outline: 1px solid #f805e3 !important;
    }

    ${t} small {
      outline: 1px solid #d107b2 !important;
    }

    ${t} abbr {
      outline: 1px solid #4a0263 !important;
    }

    ${t} q {
      outline: 1px solid #240018 !important;
    }

    ${t} cite {
      outline: 1px solid #64003c !important;
    }

    ${t} dfn {
      outline: 1px solid #b4005a !important;
    }

    ${t} sub {
      outline: 1px solid #dba0c8 !important;
    }

    ${t} sup {
      outline: 1px solid #cc0256 !important;
    }

    ${t} time {
      outline: 1px solid #d6606d !important;
    }

    ${t} code {
      outline: 1px solid #e04251 !important;
    }

    ${t} kbd {
      outline: 1px solid #5e001f !important;
    }

    ${t} samp {
      outline: 1px solid #9c0033 !important;
    }

    ${t} var {
      outline: 1px solid #d90047 !important;
    }

    ${t} mark {
      outline: 1px solid #ff0053 !important;
    }

    ${t} bdi {
      outline: 1px solid #bf3668 !important;
    }

    ${t} bdo {
      outline: 1px solid #6f1400 !important;
    }

    ${t} ruby {
      outline: 1px solid #ff7b93 !important;
    }

    ${t} rt {
      outline: 1px solid #ff2f54 !important;
    }

    ${t} rp {
      outline: 1px solid #803e49 !important;
    }

    ${t} span {
      outline: 1px solid #cc2643 !important;
    }

    ${t} br {
      outline: 1px solid #db687d !important;
    }

    ${t} wbr {
      outline: 1px solid #db175b !important;
    }`}var f=(t,o)=>{let{globals:n}=o,l=[!0,"true"].includes(n[r]),e=o.viewMode==="docs",a=u(()=>c(e?'[data-story-block="true"]':".sb-show-main"),[o]);return m(()=>{let p=e?`addon-outline-docs-${o.id}`:"addon-outline";return l?$(p,a):d(p),()=>{d(p)}},[l,a,o]),t()},b=[f],g={[r]:!1};export{b as decorators,g as globals};
