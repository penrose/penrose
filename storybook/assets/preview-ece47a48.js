let l;const T=new Uint8Array(16);function w(){if(!l&&(l=typeof crypto<"u"&&crypto.getRandomValues&&crypto.getRandomValues.bind(crypto),!l))throw new Error("crypto.getRandomValues() not supported. See https://github.com/uuidjs/uuid#getrandomvalues-not-supported");return l(T)}const i=[];for(let t=0;t<256;++t)i.push((t+256).toString(16).slice(1));function U(t,e=0){return(i[t[e+0]]+i[t[e+1]]+i[t[e+2]]+i[t[e+3]]+"-"+i[t[e+4]]+i[t[e+5]]+"-"+i[t[e+6]]+i[t[e+7]]+"-"+i[t[e+8]]+i[t[e+9]]+"-"+i[t[e+10]]+i[t[e+11]]+i[t[e+12]]+i[t[e+13]]+i[t[e+14]]+i[t[e+15]]).toLowerCase()}const x=typeof crypto<"u"&&crypto.randomUUID&&crypto.randomUUID.bind(crypto),m={randomUUID:x};function D(t,e,r){if(m.randomUUID&&!e&&!t)return m.randomUUID();t=t||{};const n=t.random||(t.rng||w)();if(n[6]=n[6]&15|64,n[8]=n[8]&63|128,e){r=r||0;for(let o=0;o<16;++o)e[r+o]=n[o];return e}return U(n)}const{addons:I}=__STORYBOOK_MODULE_PREVIEW_API__,{global:_}=__STORYBOOK_MODULE_GLOBAL__,{ImplicitActionsDuringRendering:j}=__STORYBOOK_MODULE_CORE_EVENTS_PREVIEW_ERRORS__;var v="storybook/actions",A=`${v}/action-event`,L={depth:10,clearOnStoryChange:!0,limit:50},R=(t,e)=>{let r=Object.getPrototypeOf(t);return!r||e(r)?r:R(r,e)},K=t=>!!(typeof t=="object"&&t&&R(t,e=>/^Synthetic(?:Base)?Event$/.test(e.constructor.name))&&typeof t.persist=="function"),B=t=>{if(K(t)){let e=Object.create(t.constructor.prototype,Object.getOwnPropertyDescriptors(t));e.persist();let r=Object.getOwnPropertyDescriptor(e,"view"),n=r==null?void 0:r.value;return typeof n=="object"&&(n==null?void 0:n.constructor.name)==="Window"&&Object.defineProperty(e,"view",{...r,value:Object.create(n.constructor.prototype)}),e}return t},C=()=>typeof crypto=="object"&&typeof crypto.getRandomValues=="function"?D():Date.now().toString(36)+Math.random().toString(36).substring(2);function d(t,e={}){let r={...L,...e},n=function(...o){var O,y;if(e.implicit){let g=(O="__STORYBOOK_PREVIEW__"in _?_.__STORYBOOK_PREVIEW__:void 0)==null?void 0:O.storyRenders.find(p=>p.phase==="playing"||p.phase==="rendering");if(g){let p=!((y=window==null?void 0:window.FEATURES)!=null&&y.disallowImplicitActionsInRenderV8),f=new j({phase:g.phase,name:t,deprecated:p});if(p)console.warn(f);else throw f}}let a=I.getChannel(),s=C(),c=5,u=o.map(B),b=o.length>1?u:u[0],S={id:s,count:0,data:{name:t,args:b},options:{...r,maxDepth:c+(r.depth||3),allowFunction:r.allowFunction||!1}};a.emit(A,S)};return n.isAction=!0,n.implicit=e.implicit,n}var h=(t,e)=>typeof e[t]>"u"&&!(t in e),V=t=>{let{initialArgs:e,argTypes:r,id:n,parameters:{actions:o}}=t;if(!o||o.disable||!o.argTypesRegex||!r)return{};let a=new RegExp(o.argTypesRegex);return Object.entries(r).filter(([s])=>!!a.test(s)).reduce((s,[c,u])=>(h(c,e)&&(s[c]=d(c,{implicit:!0,id:n})),s),{})},P=t=>{let{initialArgs:e,argTypes:r,parameters:{actions:n}}=t;return n!=null&&n.disable||!r?{}:Object.entries(r).filter(([o,a])=>!!a.action).reduce((o,[a,s])=>(h(a,e)&&(o[a]=d(typeof s.action=="string"?s.action:a)),o),{})},M=[P,V],E=!1,Y=t=>{let{parameters:{actions:e}}=t;if(!(e!=null&&e.disable)&&!E&&"__STORYBOOK_TEST_ON_MOCK_CALL__"in _&&typeof _.__STORYBOOK_TEST_ON_MOCK_CALL__=="function"){let r=_.__STORYBOOK_TEST_ON_MOCK_CALL__;r((n,o)=>{let a=n.getMockName();a!=="spy"&&(!/^next\/.*::/.test(a)||["next/router::useRouter()","next/navigation::useRouter()","next/navigation::redirect","next/cache::","next/headers::cookies().set","next/headers::cookies().delete","next/headers::headers().set","next/headers::headers().delete"].some(s=>a.startsWith(s)))&&d(a)(o)}),E=!0}},W=[Y];export{M as argsEnhancers,W as loaders};