var bt=Object.defineProperty;var c=(t,o)=>bt(t,"name",{value:o,configurable:!0});import{b as vt,R as xt,T as Mt,A as Ct,s as Et,m as $t,g as b,e as $,a as it,o as W,c as B,i as at,t as V,d as j,f as C,h as U,j as G,r as K,_ as Lt,k as zt,l as Nt,n as Rt,p as Tt,q as It,u as Dt,v as Vt,w as jt,x as ct,y as Ft}from"./svg-3794662c.js";import{R as Z}from"./index-74c5fbfa.js";import{j as T,a as Q}from"./jsx-runtime-9c5bc5e6.js";const Ot=c(()=>{const t=vt();xt(t);const o=new Mt({packages:Ct,macros:{textsc:["\\style{font-variant-caps: small-caps}{\\text{#1}}",1]},inlineMath:[["$","$"],["\\(","\\)"]],processEscapes:!0,formatError:(s,i)=>{throw Error(i.message)}}),n=new Et.SVG({fontCache:"none"}),e=$t.mathjax.document("",{InputJax:o,OutputJax:n});return c(s=>{try{const i=e.convert(s,{});return W(i.firstChild)}catch(i){return $(i.message)}},"convert")},"mathjaxInit"),Bt=c(t=>{const o=/^(\d+(?:\.\d+)?)\s*(px|in|cm|mm)$/,n=t.match(o);if(!n)return;const e=parseFloat(n[1]),r=n[2];return{number:e,unit:r}},"parseFontSize"),Gt=c((t,o)=>({px:1,in:96,cm:37.79527559055118,mm:3.7795275590551185})[o]*t,"toPxFontSize"),qt=c(async(t,o)=>new Promise(n=>{const e=b(t.string,""),r=b(t.fontSize,"");(r===""||e==="")&&n($(`Label 'string' and 'fontSize' must be non-empty and non-optimized for ${t.name.contents}`));const s=o(e);if(s.isErr()){n($(`MathJax could not render $${e}$: ${s.error}`));return}const i=s.value,a=i.getAttribute("viewBox");if(a===null){n($(`No ViewBox found for MathJax output $${e}$`));return}const l=a.split(" "),u=parseFloat(l[2])/1e3,d=parseFloat(l[3])/1e3,p=-parseFloat(i.style.verticalAlign),h=parseFloat(i.getAttribute("height")),k=Bt(r);if(k){const{number:m,unit:g}=k,w=c(R=>R*Gt(m,g),"em_to_px"),A=w(u),S=w(d),f=p/h*S,N=S-f;n(W({body:i,width:A,height:S,descent:f,ascent:N}))}else{n($('Invalid font size format. Only "px", "in", "cm", and "mm" units are supported.'));return}}),"tex2svg"),L=c(t=>({tag:"FloatV",contents:t}),"floatV"),tt=c((t,o,n,e)=>({tag:"TextData",width:L(t),height:L(o),descent:L(n),ascent:L(e)}),"textData"),_t=c((t,o,n,e,r)=>({tag:"EquationData",width:L(t),height:L(o),ascent:L(n),descent:L(e),rendered:r}),"equationData"),lt=c(t=>{const o=b(t.fontFamily),n=b(t.fontSize),e=b(t.fontStretch),r=b(t.fontStyle),s=b(t.fontVariant),i=b(t.fontWeight),a=b(t.lineHeight),l=`${e} ${r} ${s} ${i} ${n} ${o}`;return a!==""?l.concat(`/${a}`):l},"toFontRule"),dt=c(async(t,o)=>{const n=new Map;for(const e of t)if(e.shapeType==="Equation"){const r=b(e.name),s=await qt(e,o);if(s.isErr())return $({errorType:"SubstanceError",tag:"Fatal",message:s.error});const{body:i,width:a,height:l,ascent:u,descent:d}=s.value,p=_t(a===1/0?0:a,l===1/0?0:l,u,d,i);n.set(r,p)}else if(e.shapeType==="Text"){const r=b(e.name);let s;const i=Wt(b(e.string),lt(e));i.width&&i.height?s=tt(i.width,i.height,i.actualDescent,i.actualAscent):s=tt(0,0,0,0),n.set(r,s)}else if(e.shapeType==="Group"){const r=it(e.shapes),s=await dt(r,o);if(s.isErr())return s;for(const[i,a]of s.value.entries())n.set(i,a)}return W(n)},"collectLabels");function Wt(t,o){const n=document.createElement("canvas"),e=n.getContext("2d");e.textBaseline="alphabetic",e.font=o;const r=e.measureText(t);return n.remove(),{width:Math.abs(r.actualBoundingBoxLeft)+Math.abs(r.actualBoundingBoxRight),height:Math.abs(r.actualBoundingBoxAscent)+Math.abs(r.actualBoundingBoxDescent),actualDescent:Math.abs(r.actualBoundingBoxDescent),actualAscent:Math.abs(r.actualBoundingBoxAscent)}}c(Wt,"measureText");const E=c((t,o,n,e)=>{if(typeof n.contents!="number"&&n.contents.tag==="Input"){const{index:r,meta:s}=B(o.get(n.contents),"missing input");s.init.tag==="Pending"&&(t[r]=e.contents)}},"setPendingProperty"),ut=c((t,o,n,e)=>{for(const r of t)if(r.shapeType==="Group"){const s=it(r.shapes);ut(s,o,n,e)}else if(r.shapeType==="Equation"){const s=B(n.get(r.name.contents),"missing label");if(s.tag!=="EquationData")throw Error(`for ${r.shapeType} ${r.name.contents} got unexpected ${s.tag}`);E(o,e,r.width,s.width),E(o,e,r.height,s.height),E(o,e,r.ascent,s.ascent),E(o,e,r.descent,s.descent)}else if(r.shapeType==="Text"){const s=B(n.get(r.name.contents),"missing label");if(s.tag!=="TextData")throw Error(`for ${r.shapeType} ${r.name.contents} got unexpected ${s.tag}`);E(o,e,r.width,s.width),E(o,e,r.height,s.height),E(o,e,r.ascent,s.ascent),E(o,e,r.descent,s.descent)}},"insertPendingHelper"),pt=c(t=>{const o=[...t.varyingValues],n=new Map(t.inputs.map(({handle:e,meta:r},s)=>[e,{index:s,meta:r}]));return ut(t.shapes,o,t.labelCache,n),{...t,varyingValues:o}},"insertPending"),et={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},x=c((t,o,n)=>{const e=["strokeStyle","name","ensureOnCanvas"],r=new Set(n.concat(e));for(const[s,i]of t.passthrough)if(!(i.tag==="StrV"&&i.contents===""||r.has(s)))if(at(s,et)){const a=et[s];o.hasAttribute(a)||o.setAttribute(a,i.contents.toString())}else if(s==="style"&&i.contents!==""){const a=o.getAttribute(s);a===null?o.setAttribute(s,i.contents.toString()):o.setAttribute(s,`${a}${i.contents.toString()}`)}else o.hasAttribute(s)||o.setAttribute(s,i.contents.toString())},"attrAutoFillSvg"),z=c((t,o)=>{const n=t.fillColor,e=V(n.contents);return o.setAttribute("fill",j(n.contents)),n.contents.tag!=="NONE"&&o.setAttribute("fill-opacity",e.toString()),["fillColor"]},"attrFill"),ht=c((t,o,n)=>{const e=t.center,[r,s]=C([e.contents[0],e.contents[1]],o);return n.setAttribute("cx",r.toString()),n.setAttribute("cy",s.toString()),["center"]},"attrCenter"),ft=c((t,o)=>{let n=t.scale.contents;n=n||1;let e=o.getAttribute("transform");return e=e===null?`scale(${n})`:e+`scale{${n}}`,o.setAttribute("transform",e),["scale"]},"attrScale"),mt=c((t,o,n)=>{const e=t.center,[r,s]=C([e.contents[0],e.contents[1]],o),i=t.width,a=t.height;let l=n.getAttribute("transform");return l=l===null?`translate(${r-i.contents/2}, ${s-a.contents/2})`:l+`translate(${r-i.contents/2}, ${s-a.contents/2})`,n.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),Xt=c((t,o,n)=>{const e=t.center,[r,s]=C([e.contents[0],e.contents[1]],o),i=t.width,a=t.height;return n.setAttribute("x",(r-i.contents/2).toString()),n.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),X=c((t,o,n)=>{t.width,t.height;const e=t.center,r=t.rotation.contents,[s,i]=C([e.contents[0],e.contents[1]],o);let a=n.getAttribute("transform");return a=a===null?`rotate(${r}, ${s}, ${i})`:a+`rotate(${r}, ${s}, ${i})`,n.setAttribute("transform",a),["rotation","center","width","height"]},"attrRotation"),F=c((t,o)=>{const n=t.width,e=t.height;return o.setAttribute("width",n.contents.toString()),o.setAttribute("height",e.contents.toString()),["width","height"]},"attrWH"),Ht=c((t,o)=>{const n=t.cornerRadius;return o.setAttribute("rx",n.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Yt=c((t,o)=>{const n=t.string,e=document.createTextNode(n.contents.toString());return o.appendChild(e),["string"]},"attrString"),P="7,5",I=c((t,o)=>{const n=[],e=t.strokeColor,r=V(e.contents),s=t.strokeWidth.contents;return o.setAttribute("stroke",j(e.contents)),n.push("strokeColor","strokeWidth"),e.contents.tag!=="NONE"&&(o.setAttribute("stroke-opacity",r.toString()),o.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?o.setAttribute("stroke-dasharray",t.strokeDasharray.contents):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(o.setAttribute("stroke-dasharray",P.toString()),n.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""?o.setAttribute("stroke-linecap",t.strokeLinecap.contents):o.setAttribute("stroke-linecap","butt"),n.push("strokeLinecap")),n},"attrStroke"),M=c((t,o)=>{const n=t.name,e=document.createElementNS("http://www.w3.org/2000/svg","title");return e.textContent=n.contents,o.appendChild(e),["name"]},"attrTitle"),Ut=c((t,o)=>{const n=lt(t),e=o.getAttribute("style");return o.setAttribute("style",e?`${e}; font: ${n};`:`font: ${n};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),gt=c((t,o,n)=>{const r=t.points.contents.map(s=>C([s[0],s[1]],o));return n.setAttribute("points",r.toString()),["points"]},"attrPolyPoints"),Pt=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","circle"),e=[];return e.push(...z(t,n)),e.push(...ht(t,o,n)),e.push(...I(t,n)),e.push(...M(t,n)),n.setAttribute("r",t.r.contents.toString()),e.push("r"),x(t,n,e),n},"RenderCircle"),Jt=c((t,o,n,e)=>{const r=[...t.varyingValues];return{...t,params:U(r.length),varyingValues:r}},"dragUpdate"),Kt=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),e=[];return e.push(...z(t,n)),e.push(...ht(t,o,n)),e.push(...I(t,n)),e.push(...M(t,n)),n.setAttribute("rx",t.rx.contents.toString()),e.push("rx"),n.setAttribute("ry",t.ry.contents.toString()),e.push("ry"),x(t,n,e),n},"RenderEllipse"),nt=c((t,o,n)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text");e.textContent=t,z(n,e),F(n,e);const{center:r}=n,[s,i]=C([r.contents[0],r.contents[1]],o);return e.setAttribute("x",`${s}`),e.setAttribute("y",`${i}`),e.setAttribute("alignment-baseline","alphabetic"),e.setAttribute("dominant-baseline","alphabetic"),e.setAttribute("text-anchor","middle"),e},"placeholderString"),Zt=c((t,o)=>{const{canvasSize:n,labels:e,texLabels:r}=o;if(r)return nt(`$${b(t.string)}$`,n,t);const s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=[];i.push(...X(t,n,s)),i.push(...mt(t,n,s)),i.push(...M(t,s));const a=e.get(b(t.name));if(a&&a.tag==="EquationData"){const l=a.rendered.cloneNode(!0),u=l.getElementsByTagName("g")[0];i.push(...z(t,u)),i.push(...F(t,l)),u.setAttribute("stroke","none"),u.setAttribute("stroke-width","0");const d=t.fontSize;return l.setAttribute("style",`font-size: ${d.contents}`),s.appendChild(l),x(t,s,i),s}else return nt(b(t.string),n,t)},"RenderEquation"),Qt=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<!-- https://commons.wikimedia.org/wiki/File:Tox_hallucin.svg -->
<svg
   xmlns:dc='http://purl.org/dc/elements/1.1/'
   xmlns:cc='http://creativecommons.org/ns#'
   xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
   xmlns:svg='http://www.w3.org/2000/svg'
   xmlns='http://www.w3.org/2000/svg'
   xmlns:xlink='http://www.w3.org/1999/xlink'
   xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd'
   xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape'
   width='100%'
   height='100%'
   id='svg2'
   sodipodi:version='0.32'
 preserveAspectRatio='none'  inkscape:version='0.46'
 viewBox='0 0 300 300'  version='1.0'
   sodipodi:docname='tox hallucin.svg'
   inkscape:output_extension='org.inkscape.output.svg.inkscape'>
  <defs
     id='defs4'>
    <linearGradient
       inkscape:collect='always'
       id='linearGradient3191'>
      <stop
         style='stop-color:#ff8d00;stop-opacity:1;'
         offset='0'
         id='stop3193' />
      <stop
         style='stop-color:#ff8d00;stop-opacity:0;'
         offset='1'
         id='stop3195' />
    </linearGradient>
    <linearGradient
       id='linearGradient3175'>
      <stop
         style='stop-color:#00ccff;stop-opacity:1;'
         offset='0'
         id='stop3177' />
      <stop
         style='stop-color:#ff1300;stop-opacity:0;'
         offset='1'
         id='stop3179' />
    </linearGradient>
    <linearGradient
       id='linearGradient3155'>
      <stop
         style='stop-color:#ffffff;stop-opacity:1;'
         offset='0'
         id='stop3157' />
      <stop
         style='stop-color:#efff00;stop-opacity:1;'
         offset='1'
         id='stop3159' />
    </linearGradient>
    <inkscape:perspective
       sodipodi:type='inkscape:persp3d'
       inkscape:vp_x='0 : 526.18109 : 1'
       inkscape:vp_y='0 : 1000 : 0'
       inkscape:vp_z='744.09448 : 526.18109 : 1'
       inkscape:persp3d-origin='372.04724 : 350.78739 : 1'
       id='perspective10' />
    <radialGradient
       inkscape:collect='always'
       xlink:href='#linearGradient3155'
       id='radialGradient3161'
       cx='88.527176'
       cy='113.77536'
       fx='88.527176'
       fy='113.77536'
       r='138.2794'
       gradientUnits='userSpaceOnUse'
       gradientTransform='matrix(0.999944,-1.9657533,1.153884,0.5869605,-136.45929,210.24015)' />
    <radialGradient
       inkscape:collect='always'
       xlink:href='#linearGradient3155'
       id='radialGradient3227'
       gradientUnits='userSpaceOnUse'
       gradientTransform='matrix(0.999944,-1.9657533,1.153884,0.5869605,-136.45929,210.24015)'
       cx='88.527176'
       cy='113.77536'
       fx='88.527176'
       fy='113.77536'
       r='138.2794' />
    <radialGradient
       inkscape:collect='always'
       xlink:href='#linearGradient3191'
       id='radialGradient3197'
       cx='70.968475'
       cy='160.37096'
       fx='70.968475'
       fy='160.37096'
       r='21.348242'
       gradientTransform='matrix(1,0,0,0.9729729,0,4.3343583)'
       gradientUnits='userSpaceOnUse' />
  </defs>
  <sodipodi:namedview
     id='base'
     pagecolor='#ffffff'
     bordercolor='#666666'
     borderopacity='1.0'
     gridtolerance='10000'
     guidetolerance='10'
     objecttolerance='10'
     inkscape:pageopacity='0.0'
     inkscape:pageshadow='2'
     inkscape:zoom='1.624463'
     inkscape:cx='101.39954'
     inkscape:cy='85.047263'
     inkscape:document-units='px'
     inkscape:current-layer='layer1'
     showgrid='false'
     inkscape:window-width='1073'
     inkscape:window-height='720'
     inkscape:window-x='9'
     inkscape:window-y='37' />
  <metadata
     id='metadata7'>
    <rdf:RDF>
      <cc:Work
         rdf:about=''>
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource='http://purl.org/dc/dcmitype/StillImage' />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label='Layer 1'
     inkscape:groupmode='layer'
     id='layer1'>
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#efff00;fill-opacity:1.0;stroke:#000000;stroke-width:10;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path2383'
       sodipodi:cx='149.64484'
       sodipodi:cy='150.35516'
       sodipodi:rx='138.2794'
       sodipodi:ry='138.2794'
       d='M 287.92424,150.35516 A 138.2794,138.2794 0 1 1 11.365433,150.35516 A 138.2794,138.2794 0 1 1 287.92424,150.35516 z' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#ffffff;fill-opacity:1;stroke:#000000;stroke-width:10;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3161'
       sodipodi:cx='103.94469'
       sodipodi:cy='104.13142'
       sodipodi:rx='24.748737'
       sodipodi:ry='37.476658'
       d='M 128.69342,104.13142 A 24.748737,37.476658 0 1 1 79.19595,104.13142 A 24.748737,37.476658 0 1 1 128.69342,104.13142 z'
       transform='matrix(-0.9582289,-0.2860021,-0.2860021,0.9582289,325.97792,44.974994)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#000000;fill-opacity:1;stroke:#000000;stroke-width:10;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3167'
       sodipodi:cx='106.5'
       sodipodi:cy='114'
       sodipodi:rx='9.5'
       sodipodi:ry='10'
       d='M 116,114 A 9.5,10 0 1 1 97,114 A 9.5,10 0 1 1 116,114 z'
       transform='matrix(1.6054105,0,0,1.4584426,27.357721,-51.363012)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#ffffff;fill-opacity:1;stroke:#000000;stroke-width:10;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3168'
       sodipodi:cx='103.94469'
       sodipodi:cy='104.13142'
       sodipodi:rx='24.748737'
       sodipodi:ry='37.476658'
       d='M 128.69342,104.13142 A 24.748737,37.476658 0 1 1 79.19595,104.13142 A 24.748737,37.476658 0 1 1 128.69342,104.13142 z'
       transform='matrix(-0.9532299,0.302246,0.302246,0.9532299,175.23598,-14.157525)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#000000;fill-opacity:1;stroke:#000000;stroke-width:10;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3170'
       sodipodi:cx='106.5'
       sodipodi:cy='114'
       sodipodi:rx='9.5'
       sodipodi:ry='10'
       d='M 116,114 A 9.5,10 0 1 1 97,114 A 9.5,10 0 1 1 116,114 z'
       transform='matrix(1.5359163,0,0,1.6024784,-55.141915,-65.123921)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924528999999999;stroke:#000000;stroke-width:4.92309473;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3195'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.8086003,0,0,0.8164126,0.2640954,-11.044101)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924529;stroke:#000000;stroke-width:4.92309475;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3197'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.8086003,0,0,0.8164126,152.64503,26.616757)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924528999999999;stroke:#000000;stroke-width:4.92309473;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3199'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.8086003,0,0,0.8164126,208.91913,1.5592222)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924529;stroke:#000000;stroke-width:7.66160011;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3201'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.5225444,0,0,0.5216243,107.50785,6.167405)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924529;stroke:#000000;stroke-width:7.66160011;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3203'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.5225444,0,0,0.5216243,18.350997,91.123147)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924529;stroke:#000000;stroke-width:7.66160011;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3205'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.5225444,0,0,0.5216243,241.47652,68.717237)' />
    <path
       sodipodi:type='arc'
       style='opacity:1;fill:#00e3ff;fill-opacity:0.47924529;stroke:#000000;stroke-width:7.66160011;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1'
       id='path3207'
       sodipodi:cx='45.27861'
       sodipodi:cy='56.802517'
       sodipodi:rx='21.939119'
       sodipodi:ry='22.8727'
       d='M 67.21773,56.802517 A 21.939119,22.8727 0 1 1 23.339491,56.802517 A 21.939119,22.8727 0 1 1 67.21773,56.802517 z'
       transform='matrix(0.5225444,0,0,0.5216243,190.12964,149.00508)' />
    <path
       id='path3209'
       style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:2.7166822;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
       d='M 61.524095,141.5056 L 61.524095,150.8265 M 61.557643,163.12376 L 61.557643,172.44466 M 75.425271,156.87515 L 66.971345,156.87515 M 56.778194,157.01501 L 48.324268,157.01501 M 71.584095,145.37378 L 65.167078,152.13403 M 56.725303,161.07608 L 50.308286,167.83633 M 71.085092,166.09132 L 64.953634,160.27118 M 57.464465,153.35509 L 51.333007,147.53495' />
    <path
       id='path3221'
       style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:2.7166822;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
       d='M 86.725071,37.423625 L 86.725071,46.744525 M 86.758619,59.041785 L 86.758619,68.362685 M 100.62625,52.793175 L 92.172321,52.793175 M 81.97917,52.933035 L 73.525244,52.933035 M 96.785071,41.291805 L 90.368054,48.052055 M 81.926279,56.994105 L 75.509262,63.754355 M 96.286068,62.009345 L 90.15461,56.189205 M 82.665441,49.273115 L 76.533983,43.452975' />
    <path
       id='path3223'
       style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:2.7166822;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
       d='M 253.0319,128.4217 L 253.0319,137.7426 M 253.06544,150.03986 L 253.06544,159.36076 M 266.93307,143.79125 L 258.47915,143.79125 M 248.286,143.93111 L 239.83207,143.93111 M 263.0919,132.28988 L 256.67488,139.05013 M 248.2331,147.99218 L 241.81609,154.75243 M 262.59289,153.00742 L 256.46144,147.18728 M 248.97227,140.27119 L 242.84081,134.45105' />
    <path
       id='path3225'
       style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:2.7166822;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
       d='M 279.70375,5.2605113 L 279.70375,14.581411 M 279.73729,26.878671 L 279.73729,36.199571 M 293.60492,20.630061 L 285.151,20.630061 M 274.95784,20.769921 L 266.50392,20.769921 M 289.76375,9.1286912 L 283.34673,15.888941 M 274.90495,24.830991 L 268.48794,31.591241 M 289.26474,29.846231 L 283.13328,24.026091 M 275.64412,17.110001 L 269.51266,11.289861'
       inkscape:transform-center-x='-6.2757294'
       inkscape:transform-center-y='-7.0601956' />
    <path
       style='fill:#ffffff;fill-opacity:0.47924529;fill-rule:evenodd;stroke:#000000;stroke-width:1.2715199px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1'
       d='M 99.300775,203.95684 L 55.102321,211.07101 C 63.275993,251.4092 43.33167,297.03463 67.500148,296.25246 C 91.668625,295.4703 75.132299,239.93649 75.132299,227.42183 C 75.132299,214.90716 96.756726,203.17467 99.300775,203.95684 z'
       id='path3227'
       sodipodi:nodetypes='ccssc' />
    <path
       style='fill:#ffffff;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:10;stroke-linecap:butt;stroke-linejoin:bevel;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
       d='M 46.678979,210.37636 C 46.678979,210.37636 61.688317,182.90612 101.76017,180.50181 C 148.43916,177.70107 158.61785,218.03349 198.85245,218.77857 C 250.19932,219.71215 256.73439,187.03686 256.73439,187.03686 L 257.66797,186.10329 C 255.4936,218.71891 214.56016,233.58514 186.79701,232.12826 C 154.98692,230.45901 145.34234,210.03157 116.69745,202.90772 C 90.073723,196.28652 46.678979,210.37636 46.678979,210.37636 z'
       id='path3193'
       sodipodi:nodetypes='cssccssc' />
  </g>
</svg>`;let te=1;const ee=c((t,o)=>{const n="--inject-",e={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},r=n+te++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const l=o?new Set:void 0,u=new Set,d=[];let p=!1,h,k;if(i.length){for(h=0;h<i.length;h++){const y=i[h].localName;at(y,e)&&u.add(y)}u.forEach(y=>{(e[y]||[y]).forEach(function(f){d.indexOf(f)<0&&d.push(f)})}),d.length&&d.push("style");const m=t.getElementsByTagName("*");let g=t,w,A,S;for(h=-1;g!==null;){if(g.localName==="style")A=g.textContent,S=A&&A.replace(s,function(y,f){return l&&l.add(f),"url(#"+f+r+")"}),S!==A&&(g.textContent=S);else if(g.hasAttributes()){for(k=0;k<d.length;k++)w=d[k],A=g.getAttribute(w),S=A&&A.replace(s,function(y,f){return l&&l.add(f),"url(#"+f+r+")"}),S&&S!==A&&g.setAttribute(w,S);for(const y of["xlink:href","href"]){let f=g.getAttribute(y);f&&/^\s*#/.test(f)&&(f=f.trim(),g.setAttribute(y,f+r),l&&l.add(f.substring(1)))}}g=m.item(++h)}for(h=0;h<i.length;h++)a=i[h],(!l||l.has(a.id))&&(a.id+=r,p=!0)}return p},"makeIdsUnique"),ne=c(async(t,{canvasSize:o,pathResolver:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),r=[],s=t.href.contents;let i=await n(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=Qt),r.push("href"),e.innerHTML=i;const a=e.querySelector("svg");return ee(e,!1),r.push(...F(t,a)),r.push(...X(t,o,e)),r.push(...mt(t,o,e)),r.push(...M(t,e)),x(t,e,r),e},"RenderImage"),q=c((t,o,n,e,r,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",K(e.width*r).toString()),i.setAttribute("markerHeight",K(e.height*r).toString()),i.setAttribute("viewBox",e.viewbox),i.setAttribute("refX",e.refX.toString()),i.setAttribute("refY",e.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",e.path),e.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",o),i.setAttribute("stroke-opacity",n.toString())):(a.setAttribute("fill",o),a.setAttribute("fill-opacity",n.toString())),e.style&&Object.entries(e.style).forEach(([l,u])=>{a.setAttribute(l,u)}),i.appendChild(a),i},"arrowHead"),oe=c((t,o,n)=>{const e=[],[r,s]=[t.start.contents[0],t.start.contents[1]],[i,a]=[t.end.contents[0],t.end.contents[1]],l=t.startArrowheadSize.contents,u=t.endArrowheadSize.contents,d=t.strokeWidth.contents;e.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const p=Math.sqrt((r-i)**2+(s-a)**2);let h,k;if(o){const A=(t.flipStartArrowhead.contents?o.refX:o.width-o.refX)*l*d,S=A/p*(r-i),y=A/p*(s-a);[h,k]=[r-S,s-y]}else[h,k]=[r,s];let m,g;if(n){const w=(n.width-n.refX)*u*d;[m,g]=[i-w/p*(i-r),a-w/p*(a-s)]}else[m,g]=[i,a];return[[[h,k],[m,g]],e]},"makeRoomForArrows"),re=c((t,{canvasSize:o,namespace:n,variation:e})=>{const r=G(t.startArrowhead.contents),s=G(t.endArrowhead.contents),[[[i,a],[l,u]],d]=oe(t,r,s),[p,h]=C([i,a],o),[k,m]=C([l,u],o),g=`M ${p} ${h} L ${k} ${m}`,w=j(t.strokeColor.contents),A=t.strokeWidth.contents,S=V(t.strokeColor.contents),y=document.createElementNS("http://www.w3.org/2000/svg","g"),f=`${n}-${e}-${t.name.contents}`,N=f+"-startArrowId",R=f+"-endArrowId";if(r){const D=t.startArrowheadSize.contents,H=t.flipStartArrowhead.contents;y.appendChild(q(N,w,S,r,D,H))}if(s){const D=t.endArrowheadSize.contents;y.appendChild(q(R,w,S,s,D,!1))}d.push("strokeColor","strokeWidth","startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize");const v=document.createElementNS("http://www.w3.org/2000/svg","path");return v.setAttribute("d",g),t.strokeColor.contents.tag!=="NONE"&&(v.setAttribute("stroke-opacity",S.toString()),v.setAttribute("stroke-width",A.toString())),v.setAttribute("stroke",w),t.strokeDasharray.contents!==""?v.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&v.setAttribute("stroke-dasharray",P.toString()),d.push("strokeDasharray","strokeStyle"),t.strokeLinecap.contents!==""?v.setAttribute("stroke-linecap",t.strokeLinecap.contents):v.setAttribute("stroke-linecap","butt"),d.push("strokeLinecap"),r&&(v.setAttribute("marker-start",`url(#${N})`),d.push("startArrowhead")),s&&(v.setAttribute("marker-end",`url(#${R})`),d.push("endArrowhead")),y.appendChild(v),d.push(...M(t,y)),x(t,y,d),y},"RenderLine"),se=c((t,o)=>t.map(n=>{const{cmd:e,contents:r}=n;if(r.length===0&&e!=="Z")return console.error("WARNING: empty path"),"";const s=Lt.flatten(r.map(i=>{switch(i.tag){case"CoordV":return C([i.contents[0],i.contents[1]],o);case"ValueV":return i.contents}})).join(" ");return`${e} ${s}`}).join(" "),"toPathString"),ie=c(t=>{const o=document.createElementNS("http://www.w3.org/2000/svg","filter");return o.setAttribute("id",t),o.setAttribute("x","0"),o.setAttribute("y","0"),o.setAttribute("width","200%"),o.setAttribute("height","200%"),o.innerHTML=`
    <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5" />
       <feGaussianBlur result="blurOut" in="offOut" stdDeviation="4" />
       <feBlend in="SourceGraphic" in2="blurOut" mode="normal" />
       <feComponentTransfer>
         <feFuncA type="linear" slope="0.5" />
       </feComponentTransfer>
       <feMerge>
         <feMergeNode />
         <feMergeNode in="SourceGraphic" />
       </feMerge>
    `,o},"Shadow"),ae=c((t,{canvasSize:o})=>{const n=t.name.contents+"-startArrowId",e=t.name.contents+"-endArrowId",r=t.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=t.strokeWidth.contents,a=j(t.strokeColor.contents),l=V(t.strokeColor.contents),u=j(t.fillColor.contents),d=V(t.fillColor.contents),p=[],h=G(t.startArrowhead.contents),k=G(t.endArrowhead.contents);if(h){const g=t.name.contents+"-startArrowId",w=t.startArrowheadSize.contents,A=t.flipStartArrowhead.contents;s.appendChild(q(g,a,l,h,w,A))}if(k){const g=t.name.contents+"-endArrowId",w=t.endArrowheadSize.contents;s.appendChild(q(g,a,l,k,w,!1))}p.push("name","strokeColor","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(ie(r));const m=document.createElementNS("http://www.w3.org/2000/svg","path");return m.setAttribute("stroke",a),m.setAttribute("fill",u),p.push("fillColor","strokeColor"),t.strokeColor.contents.tag!=="NONE"&&(m.setAttribute("stroke-width",i.toString()),m.setAttribute("stroke-opacity",l.toString()),p.push("strokeColor","strokeWidth")),t.fillColor.contents.tag!=="NONE"&&(m.setAttribute("fill-opacity",d.toString()),p.push("fillColor")),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?m.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&m.setAttribute("stroke-dasharray",P.toString()),p.push("strokeDasharray","strokeStyle"),m.setAttribute("d",se(t.d.contents,o)),p.push("d"),h&&(m.setAttribute("marker-start",`url(#${n})`),p.push("startArrowhead")),k&&(m.setAttribute("marker-end",`url(#${e})`),p.push("endArrowhead")),s.appendChild(m),p.push(...M(t,s)),x(t,s,p),s},"RenderPath"),ce=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","polygon"),e=[];return e.push(...z(t,n)),e.push(...I(t,n)),e.push(...M(t,n)),e.push(...ft(t,n)),e.push(...gt(t,o,n)),x(t,n,e),n},"RenderPolygon"),le=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","polyline"),e=[];return e.push(...z(t,n)),e.push(...I(t,n)),e.push(...M(t,n)),e.push(...ft(t,n)),e.push(...gt(t,o,n)),x(t,n,e),n},"RenderPolyline"),de=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","rect"),e=[];return e.push(...Xt(t,o,n)),e.push(...F(t,n)),e.push(...z(t,n)),e.push(...I(t,n)),e.push(...M(t,n)),e.push(...Ht(t,n)),e.push(...X(t,o,n)),x(t,n,e),n},"RenderRectangle"),ue=c((t,{canvasSize:o,labels:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text"),r=[];r.push("x","y"),r.push(...z(t,e)),r.push(...I(t,e)),r.push(...M(t,e)),r.push(...Yt(t,e)),r.push(...X(t,o,e)),r.push(...Ut(t,e));const s=t.name,i=n.get(s.contents),a=t.center,[l,u]=C([a.contents[0],a.contents[1]],o);if(i&&i.tag==="TextData"){const d=i.descent.contents,p=i.height.contents,h=u+(p/2-d);e.setAttribute("x",l.toString()),e.setAttribute("y",h.toString()),r.push(...F(t,e))}else e.setAttribute("x",l.toString()),e.setAttribute("y",u.toString());return e.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),e.setAttribute("alignment-baseline",t.alignmentBaseline.contents),e.setAttribute("dominant-baseline",t.dominantBaseline.contents),e.setAttribute("ascent",t.ascent.contents.toString()),e.setAttribute("descent",t.descent.contents.toString()),e.setAttribute("text-anchor",t.textAnchor.contents.toString()),e.setAttribute("visibility",t.visibility.contents),r.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),x(t,e,r),e},"RenderText"),ot=c(({clientX:t,clientY:o},n)=>{const e=n.getScreenCTM();return e!==null?{x:(t-e.e)/e.a,y:(o-e.f)/e.d}:{x:0,y:0}},"getPosition"),pe=c(async(t,o,n,e)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","svg");r.setAttribute("xmlns","http://www.w3.org/2000/svg"),r.setAttribute("version","1.2"),r.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((a,l,u)=>{o(Jt(t))},"onDrag"),i=t.computeShapes(t.varyingValues);return await yt(i,r,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:e,texLabels:!1,pathResolver:n},{updateState:o,onDrag:s,parentSVG:r}),r},"RenderInteractive"),he=c(async(t,o,n,e=!1)=>{const{varyingValues:r,computeShapes:s,labelCache:i,canvas:a,variation:l}=t,u=document.createElementNS("http://www.w3.org/2000/svg","svg");u.setAttribute("version","1.2"),u.setAttribute("xmlns","http://www.w3.org/2000/svg"),u.setAttribute("viewBox",`0 0 ${a.width} ${a.height}`);const d=s(r);return await yt(d,u,{labels:i,canvasSize:a.size,variation:l,namespace:n,texLabels:e,pathResolver:o},void 0),u},"RenderStatic"),fe=c(async(t,o,n)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),r=t.clipPath.contents;let s,i;if(r.tag==="Clip"){const l=r.contents;s=l.name.contents;const u=await O(l,o,n),d=document.createElementNS("http://www.w3.org/2000/svg","clipPath");i=o.namespace+s+"-clip",d.setAttribute("id",i),d.appendChild(u),e.appendChild(d)}const a=t.shapes.contents;for(const l of a){const u=l.name.contents;if(r.tag==="Clip"){if(u!==s){const d=await O(l,o,n);d.setAttribute("clip-path",`url(#${i})`),e.appendChild(d)}}else{const d=await O(l,o,n);e.appendChild(d)}}return x(t,e,[...M(t,e),"shapes","clipPath"]),e},"RenderGroup"),me=c(async(t,o)=>{switch(t.shapeType){case"Circle":return Pt(t,o);case"Ellipse":return Kt(t,o);case"Equation":return Zt(t,o);case"Image":return ne(t,o);case"Line":return re(t,o);case"Path":return ae(t,o);case"Polygon":return ce(t,o);case"Polyline":return le(t,o);case"Rectangle":return de(t,o);case"Text":return ue(t,o)}},"RenderShapeSvg"),O=c(async(t,o,n)=>{if(t.shapeType==="Group")return await fe(t,o,n);{const e=await me(t,o);if(n){const r=document.createElementNS("http://www.w3.org/2000/svg","g");zt(t)?r.setAttribute("pointer-events","visibleStroke"):Nt(t)?r.setAttribute("pointer-events","bounding-box"):r.setAttribute("pointer-events","auto"),r.appendChild(e);const s=c(i=>{const{clientX:a,clientY:l}=i,{x:u,y:d}=ot({clientX:a,clientY:l},n.parentSVG),{width:p,height:h,x:k,y:m}=i.target.getBBox({stroke:!0}),g=u-k,w=o.canvasSize[0]-p+(u-k),A=d-m,S=o.canvasSize[1]-h+(d-m);r.setAttribute("opacity","0.5");let y=0,f=0;const N=c(v=>{const{x:D,y:H}=ot(v,n.parentSVG),At=rt(D,g,w),St=rt(H,A,S);y=At-u,f=d-St,r.setAttribute("transform",`translate(${y},${-f})`)},"onMouseMove"),R=c(()=>{r.setAttribute("opacity","1"),document.removeEventListener("mouseup",R),document.removeEventListener("mousemove",N),n.onDrag(t.name.contents,y,f)},"onMouseUp");document.addEventListener("mouseup",R),document.addEventListener("mousemove",N)},"onMouseDown");return r.addEventListener("mousedown",s),r}else return e}},"RenderShape"),yt=c(async(t,o,n,e)=>{for(const r of t){const s=await O(r,n,e);o.appendChild(s)}},"RenderShapes"),rt=c((t,o,n)=>Math.min(Math.max(t,o),n),"clamp"),ge=c(t=>{const o=Rt(t.variation);return pt({...t,varyingValues:t.inputs.map(({meta:n})=>n.init.tag==="Sampled"?n.init.sampler(o):n.init.pending),currentStageIndex:0,params:U(t.varyingValues.length)})},"resample"),ye=c((t,o)=>{const{constraintSets:n,optStages:e,currentStageIndex:r}=t,s=e[r],i=B(n.get(s),"missing stage"),a=new Float64Array(t.varyingValues);let l=0;const u=Ft((d,p,h)=>t.gradient(i,d,p,h).phi,a,t.params,()=>l++>=o);return{...t,varyingValues:Array.from(a),params:u}},"step"),kt=c((t,o=1e4)=>{const n=ye(t,o);return _(n)&&!J(n)?wt(n):n},"stepState"),wt=c(t=>J(t)?t:{...t,currentStageIndex:t.currentStageIndex+1,params:U(t.varyingValues.length)},"nextStage"),ke=c((t,o=1e4)=>{let n=t;for(;n.params.optStatus!=="Error"&&(!_(n)||!J(n));)_(n)&&(n=wt(n)),n=kt(n,o);return n.params.optStatus==="Error"?$({errorType:"RuntimeError",...Tt("",n)}):W(n)},"stepUntilConvergence"),we=c(async t=>{const o=It(t.domain),n=Dt(r=>Vt(t.substance,r),o);return n.isErr()?$(n.error):await jt(t.variation,t.style,...n.value)},"compileTrio"),Ae=c(async t=>{const o=Ot(),n=await dt(t.shapes,o);if(n.isErr())throw Error(ct(n.error));return pt({...t,labelCache:n.value})},"prepareState"),_=c(t=>t.params.optStatus==="EPConverged","stateConverged"),J=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function st(t){const o=await fetch(t);if(!o.ok){console.error(`could not fetch ${t}`);return}return await o.text()}c(st,"fetchResolver");class Y extends Z.Component{canvasRef=Z.createRef();penroseState=void 0;timerID=void 0;constructor(o){super(o),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const o=await we(this.props);o.isOk()?(this.penroseState=await Ae(o.value),this.setState({error:void 0})):this.setState({error:o.error})};converge=async()=>{if(this.penroseState){const o=ke(this.penroseState);o.isOk()?this.penroseState=o.value:this.setState({error:o.error})}};tick=()=>{this.props.animate&&this.penroseState&&!_(this.penroseState)&&(this.penroseState=kt(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async o=>{if(this.props.domain!==o.domain||this.props.substance!==o.substance||this.props.style!==o.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==o.variation||this.props.animate!==o.animate){this.penroseState.variation=this.props.variation,this.penroseState=ge(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==o.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return T("div",{children:"rendering..."});{const o=this.canvasRef.current;if(this.penroseState){const n=await(this.props.interactive===!1?he(this.penroseState,this.props.imageResolver??st,this.props.name??""):pe(this.penroseState,async e=>{this.penroseState=e,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??st,this.props.name??""));o.firstChild!==null?o.replaceChild(n,o.firstChild):o.appendChild(n),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return T("div",{children:"rendering..."})}};render=()=>{const{error:o}=this.state;return Q("div",{style:{width:"100%",height:"100%"},children:[!o&&T("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),o&&Q("div",{style:{padding:"1em",height:"100%"},children:[T("div",{style:{fontWeight:700},children:"1 error:"}),T("div",{style:{fontFamily:"monospace"},children:ct(o).toString().split(`
`).map((n,e)=>T("p",{style:{margin:0},children:n},`err-ln-${e}`))})]})]})}}c(Y,"Simple");try{Y.displayName="Simple",Y.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{Y as S};
//# sourceMappingURL=Simple-9d6bde10.js.map
