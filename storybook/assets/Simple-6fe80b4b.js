var bt=Object.defineProperty;var c=(t,o)=>bt(t,"name",{value:o,configurable:!0});import{b as xt,R as Mt,T as Ct,A as Et,s as $t,m as Lt,g as v,e as $,a as H,o as _,c as B,i as it,t as V,d as j,f as C,h as U,j as G,r as K,_ as Tt,k as Nt,l as Rt,n as zt,p as It,q as Dt,u as Vt,v as jt,w as Ot,x as at,y as Bt}from"./svg-56fd447a.js";import{R as Z}from"./index-74c5fbfa.js";import{j as L,a as Q}from"./jsx-runtime-9c5bc5e6.js";const ct=10,Gt=c(()=>{const t=xt();Mt(t);const o=new Ct({packages:Et,macros:{textsc:["\\style{font-variant-caps: small-caps}{\\text{#1}}",1]},inlineMath:[["$","$"],["\\(","\\)"]],processEscapes:!0,formatError:(s,i)=>{throw Error(i.message)}}),n=new $t.SVG({fontCache:"none"}),e=Lt.mathjax.document("",{InputJax:o,OutputJax:n});return c((s,i)=>{const a=`\\displaylines{${s}}`;try{const l=e.convert(a,{ex:ct});return t.setStyle(l,"font-size",i),_(l.firstChild)}catch(l){return $(l.message)}},"convert")},"mathjaxInit"),Ft=c(async(t,o)=>new Promise(n=>{const e=v(t.string,""),r=v(t.fontSize,"");(r===""||e==="")&&n($(`Label 'string' and 'fontSize' must be non-empty and non-optimized for ${t.name.contents}`));const s=o(e,r);if(s.isErr()){n($(`MathJax could not render $${e}$: ${s.error}`));return}const i=s.value,a=i.getAttribute("viewBox");if(a===null){n($(`No ViewBox found for MathJax output $${e}$`));return}const l=a.split(" "),d=parseFloat(l[2]),u=parseFloat(l[3]),h=parseFloat(i.style.verticalAlign)*ct,p=parseFloat(r)-h,k=p/u*d;n(_({body:i,width:k,height:p}))}),"tex2svg"),N=c(t=>({tag:"FloatV",contents:t}),"floatV"),tt=c((t,o,n,e)=>({tag:"TextData",width:N(t),height:N(o),descent:N(n),ascent:N(e)}),"textData"),qt=c((t,o,n)=>({tag:"EquationData",width:N(t),height:N(o),rendered:n}),"equationData"),lt=c(t=>{const o=v(t.fontFamily),n=v(t.fontSize),e=v(t.fontStretch),r=v(t.fontStyle),s=v(t.fontVariant),i=v(t.fontWeight),a=v(t.lineHeight),l=`${e} ${r} ${s} ${i} ${n} ${o}`;return a!==""?l.concat(`/${a}`):l},"toFontRule"),dt=c(async(t,o)=>{const n=new Map;for(const e of t)if(e.shapeType==="Equation"){const r=v(e.name),s=await Ft(e,o);if(s.isErr())return $({errorType:"SubstanceError",tag:"Fatal",message:s.error});const{body:i,width:a,height:l}=s.value,d=qt(a===1/0?0:a,l===1/0?0:l,i);n.set(r,d)}else if(e.shapeType==="Text"){const r=v(e.name);let s;const i=_t(v(e.string),lt(e));i.width&&i.height?s=tt(i.width,i.height,i.actualDescent,i.actualAscent):s=tt(0,0,0,0),n.set(r,s)}else if(e.shapeType==="Group"){const r=H(e.shapes),s=await dt(r,o);if(s.isErr())return s;for(const[i,a]of s.value.entries())n.set(i,a)}return _(n)},"collectLabels");function _t(t,o){const n=document.createElement("canvas"),e=n.getContext("2d");e.textBaseline="alphabetic",e.font=o;const r=e.measureText(t);return n.remove(),{width:Math.abs(r.actualBoundingBoxLeft)+Math.abs(r.actualBoundingBoxRight),height:Math.abs(r.actualBoundingBoxAscent)+Math.abs(r.actualBoundingBoxDescent),actualDescent:Math.abs(r.actualBoundingBoxDescent),actualAscent:Math.abs(r.actualBoundingBoxAscent)}}c(_t,"measureText");const T=c((t,o,n,e)=>{if(typeof n.contents!="number"&&n.contents.tag==="Input"){const{index:r,meta:s}=B(o.get(n.contents),"missing input");s.init.tag==="Pending"&&(t[r]=e.contents)}},"setPendingProperty"),ut=c((t,o,n,e)=>{for(const r of t)if(r.shapeType==="Group"){const s=H(r.shapes);ut(s,o,n,e)}else if(r.shapeType==="Equation"){const s=B(n.get(r.name.contents),"missing label");if(s.tag!=="EquationData")throw Error(`for ${r.shapeType} ${r.name.contents} got unexpected ${s.tag}`);T(o,e,r.width,s.width),T(o,e,r.height,s.height)}else if(r.shapeType==="Text"){const s=B(n.get(r.name.contents),"missing label");if(s.tag!=="TextData")throw Error(`for ${r.shapeType} ${r.name.contents} got unexpected ${s.tag}`);T(o,e,r.width,s.width),T(o,e,r.height,s.height),T(o,e,r.ascent,s.ascent),T(o,e,r.descent,s.descent)}},"insertPendingHelper"),pt=c(t=>{const o=[...t.varyingValues],n=new Map(t.inputs.map(({handle:e,meta:r},s)=>[e,{index:s,meta:r}]));return ut(t.shapes,o,t.labelCache,n),{...t,varyingValues:o}},"insertPending"),et={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},x=c((t,o,n)=>{const e=["strokeStyle","name","ensureOnCanvas"],r=new Set(n.concat(e));for(const[s,i]of t.passthrough)if(!(i.tag==="StrV"&&i.contents===""||r.has(s)))if(it(s,et)){const a=et[s];o.hasAttribute(a)||o.setAttribute(a,i.contents.toString())}else if(s==="style"&&i.contents!==""){const a=o.getAttribute(s);a===null?o.setAttribute(s,i.contents.toString()):o.setAttribute(s,`${a}${i.contents.toString()}`)}else o.hasAttribute(s)||o.setAttribute(s,i.contents.toString())},"attrAutoFillSvg"),E=c((t,o)=>{const n=t.fillColor,e=V(n.contents);return o.setAttribute("fill",j(n.contents)),n.contents.tag!=="NONE"&&o.setAttribute("fill-opacity",e.toString()),["fillColor"]},"attrFill"),ht=c((t,o,n)=>{const e=t.center,[r,s]=C([e.contents[0],e.contents[1]],o);return n.setAttribute("cx",r.toString()),n.setAttribute("cy",s.toString()),["center"]},"attrCenter"),ft=c((t,o)=>{let n=t.scale.contents;n=n||1;let e=o.getAttribute("transform");return e=e===null?`scale(${n})`:e+`scale{${n}}`,o.setAttribute("transform",e),["scale"]},"attrScale"),gt=c((t,o,n)=>{const e=t.center,[r,s]=C([e.contents[0],e.contents[1]],o),i=t.width,a=t.height;let l=n.getAttribute("transform");return l=l===null?`translate(${r-i.contents/2}, ${s-a.contents/2})`:l+`translate(${r-i.contents/2}, ${s-a.contents/2})`,n.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),Wt=c((t,o,n)=>{const e=t.center,[r,s]=C([e.contents[0],e.contents[1]],o),i=t.width,a=t.height;return n.setAttribute("x",(r-i.contents/2).toString()),n.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),W=c((t,o,n)=>{t.width,t.height;const e=t.center,r=t.rotation.contents,[s,i]=C([e.contents[0],e.contents[1]],o);let a=n.getAttribute("transform");return a=a===null?`rotate(${r}, ${s}, ${i})`:a+`rotate(${r}, ${s}, ${i})`,n.setAttribute("transform",a),["rotation","center","width","height"]},"attrRotation"),O=c((t,o)=>{const n=t.width,e=t.height;return o.setAttribute("width",n.contents.toString()),o.setAttribute("height",e.contents.toString()),["width","height"]},"attrWH"),Xt=c((t,o)=>{const n=t.cornerRadius;return o.setAttribute("rx",n.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Yt=c((t,o)=>{const n=t.string,e=document.createTextNode(n.contents.toString());return o.appendChild(e),["string"]},"attrString"),P="7,5",R=c((t,o)=>{const n=[],e=t.strokeColor,r=V(e.contents),s=t.strokeWidth.contents;return o.setAttribute("stroke",j(e.contents)),n.push("strokeColor","strokeWidth"),e.contents.tag!=="NONE"&&(o.setAttribute("stroke-opacity",r.toString()),o.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?o.setAttribute("stroke-dasharray",t.strokeDasharray.contents):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(o.setAttribute("stroke-dasharray",P.toString()),n.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""?o.setAttribute("stroke-linecap",t.strokeLinecap.contents):o.setAttribute("stroke-linecap","butt"),n.push("strokeLinecap")),n},"attrStroke"),M=c((t,o)=>{const n=t.name,e=document.createElementNS("http://www.w3.org/2000/svg","title");return e.textContent=n.contents,o.appendChild(e),["name"]},"attrTitle"),Ht=c((t,o)=>{const n=lt(t),e=o.getAttribute("style");return o.setAttribute("style",e?`${e}; font: ${n};`:`font: ${n};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),mt=c((t,o,n)=>{const r=t.points.contents.map(s=>C([s[0],s[1]],o));return n.setAttribute("points",r.toString()),["points"]},"attrPolyPoints"),Ut=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","circle"),e=[];return e.push(...E(t,n)),e.push(...ht(t,o,n)),e.push(...R(t,n)),e.push(...M(t,n)),n.setAttribute("r",t.r.contents.toString()),e.push("r"),x(t,n,e),n},"RenderCircle"),Pt=c((t,o,n,e)=>{const r=[...t.varyingValues];return{...t,params:U(r.length),varyingValues:r}},"dragUpdate"),Jt=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),e=[];return e.push(...E(t,n)),e.push(...ht(t,o,n)),e.push(...R(t,n)),e.push(...M(t,n)),n.setAttribute("rx",t.rx.contents.toString()),e.push("rx"),n.setAttribute("ry",t.ry.contents.toString()),e.push("ry"),x(t,n,e),n},"RenderEllipse"),nt=c((t,o,n)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text");e.textContent=t,E(n,e),O(n,e);const{center:r}=n,[s,i]=C([r.contents[0],r.contents[1]],o);return e.setAttribute("x",`${s}`),e.setAttribute("y",`${i}`),e.setAttribute("alignment-baseline","alphabetic"),e.setAttribute("dominant-baseline","alphabetic"),e.setAttribute("text-anchor","middle"),e},"placeholderString"),Kt=c((t,o)=>{const{canvasSize:n,labels:e,texLabels:r}=o;if(r)return nt(`$${v(t.string)}$`,n,t);const s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=[];i.push(...W(t,n,s)),i.push(...gt(t,n,s)),i.push(...M(t,s));const a=e.get(v(t.name));if(a&&a.tag==="EquationData"){const l=a.rendered.cloneNode(!0),d=l.getElementsByTagName("g")[0];i.push(...E(t,d)),i.push(...O(t,l)),d.setAttribute("stroke","none"),d.setAttribute("stroke-width","0");const u=t.fontSize;return l.setAttribute("style",`font-size: ${u.contents}`),s.appendChild(l),x(t,s,i),s}else return nt(v(t.string),n,t)},"RenderEquation"),Zt=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let Qt=1;const te=c((t,o)=>{const n="--inject-",e={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},r=n+Qt++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const l=o?new Set:void 0,d=new Set,u=[];let h=!1,p,k;if(i.length){for(p=0;p<i.length;p++){const f=i[p].localName;it(f,e)&&d.add(f)}d.forEach(f=>{(e[f]||[f]).forEach(function(y){u.indexOf(y)<0&&u.push(y)})}),u.length&&u.push("style");const g=t.getElementsByTagName("*");let m=t,w,A,S;for(p=-1;m!==null;){if(m.localName==="style")A=m.textContent,S=A&&A.replace(s,function(f,y){return l&&l.add(y),"url(#"+y+r+")"}),S!==A&&(m.textContent=S);else if(m.hasAttributes()){for(k=0;k<u.length;k++)w=u[k],A=m.getAttribute(w),S=A&&A.replace(s,function(f,y){return l&&l.add(y),"url(#"+y+r+")"}),S&&S!==A&&m.setAttribute(w,S);for(const f of["xlink:href","href"]){let y=m.getAttribute(f);y&&/^\s*#/.test(y)&&(y=y.trim(),m.setAttribute(f,y+r),l&&l.add(y.substring(1)))}}m=g.item(++p)}for(p=0;p<i.length;p++)a=i[p],(!l||l.has(a.id))&&(a.id+=r,h=!0)}return h},"makeIdsUnique"),ee=c(async(t,{canvasSize:o,pathResolver:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),r=[],s=t.href.contents;let i=await n(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=Zt),r.push("href"),e.innerHTML=i;const a=e.querySelector("svg");return te(e,!1),r.push(...O(t,a)),r.push(...W(t,o,e)),r.push(...gt(t,o,e)),r.push(...M(t,e)),x(t,e,r),e},"RenderImage"),F=c((t,o,n,e,r,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",K(e.width*r).toString()),i.setAttribute("markerHeight",K(e.height*r).toString()),i.setAttribute("viewBox",e.viewbox),i.setAttribute("refX",e.refX.toString()),i.setAttribute("refY",e.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",e.path),e.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",o),i.setAttribute("stroke-opacity",n.toString())):(a.setAttribute("fill",o),a.setAttribute("fill-opacity",n.toString())),e.style&&Object.entries(e.style).forEach(([l,d])=>{a.setAttribute(l,d)}),i.appendChild(a),i},"arrowHead"),ne=c((t,o,n)=>{const e=[],[r,s]=[t.start.contents[0],t.start.contents[1]],[i,a]=[t.end.contents[0],t.end.contents[1]],l=t.startArrowheadSize.contents,d=t.endArrowheadSize.contents,u=t.strokeWidth.contents;e.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const h=Math.sqrt((r-i)**2+(s-a)**2);let p,k;if(o){const A=(t.flipStartArrowhead.contents?o.refX:o.width-o.refX)*l*u,S=A/h*(r-i),f=A/h*(s-a);[p,k]=[r-S,s-f]}else[p,k]=[r,s];let g,m;if(n){const w=(n.width-n.refX)*d*u;[g,m]=[i-w/h*(i-r),a-w/h*(a-s)]}else[g,m]=[i,a];return[[[p,k],[g,m]],e]},"makeRoomForArrows"),oe=c((t,{canvasSize:o,namespace:n,variation:e})=>{const r=G(t.startArrowhead.contents),s=G(t.endArrowhead.contents),[[[i,a],[l,d]],u]=ne(t,r,s),[h,p]=C([i,a],o),[k,g]=C([l,d],o),m=`M ${h} ${p} L ${k} ${g}`,w=j(t.strokeColor.contents),A=t.strokeWidth.contents,S=V(t.strokeColor.contents),f=document.createElementNS("http://www.w3.org/2000/svg","g"),y=`${n}-${e}-${t.name.contents}`,z=y+"-startArrowId",I=y+"-endArrowId";if(r){const D=t.startArrowheadSize.contents,X=t.flipStartArrowhead.contents;f.appendChild(F(z,w,S,r,D,X))}if(s){const D=t.endArrowheadSize.contents;f.appendChild(F(I,w,S,s,D,!1))}u.push("strokeColor","strokeWidth","startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize");const b=document.createElementNS("http://www.w3.org/2000/svg","path");return b.setAttribute("d",m),t.strokeColor.contents.tag!=="NONE"&&(b.setAttribute("stroke-opacity",S.toString()),b.setAttribute("stroke-width",A.toString())),b.setAttribute("stroke",w),t.strokeDasharray.contents!==""?b.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&b.setAttribute("stroke-dasharray",P.toString()),u.push("strokeDasharray","strokeStyle"),t.strokeLinecap.contents!==""?b.setAttribute("stroke-linecap",t.strokeLinecap.contents):b.setAttribute("stroke-linecap","butt"),u.push("strokeLinecap"),r&&(b.setAttribute("marker-start",`url(#${z})`),u.push("startArrowhead")),s&&(b.setAttribute("marker-end",`url(#${I})`),u.push("endArrowhead")),f.appendChild(b),u.push(...M(t,f)),x(t,f,u),f},"RenderLine"),re=c((t,o)=>t.map(n=>{const{cmd:e,contents:r}=n;if(r.length===0&&e!=="Z")return console.error("WARNING: empty path"),"";const s=Tt.flatten(r.map(i=>{switch(i.tag){case"CoordV":return C([i.contents[0],i.contents[1]],o);case"ValueV":return i.contents}})).join(" ");return`${e} ${s}`}).join(" "),"toPathString"),se=c(t=>{const o=document.createElementNS("http://www.w3.org/2000/svg","filter");return o.setAttribute("id",t),o.setAttribute("x","0"),o.setAttribute("y","0"),o.setAttribute("width","200%"),o.setAttribute("height","200%"),o.innerHTML=`
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
    `,o},"Shadow"),ie=c((t,{canvasSize:o})=>{const n=t.name.contents+"-startArrowId",e=t.name.contents+"-endArrowId",r=t.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=t.strokeWidth.contents,a=j(t.strokeColor.contents),l=V(t.strokeColor.contents),d=j(t.fillColor.contents),u=V(t.fillColor.contents),h=[],p=G(t.startArrowhead.contents),k=G(t.endArrowhead.contents);if(p){const m=t.name.contents+"-startArrowId",w=t.startArrowheadSize.contents,A=t.flipStartArrowhead.contents;s.appendChild(F(m,a,l,p,w,A))}if(k){const m=t.name.contents+"-endArrowId",w=t.endArrowheadSize.contents;s.appendChild(F(m,a,l,k,w,!1))}h.push("name","strokeColor","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(se(r));const g=document.createElementNS("http://www.w3.org/2000/svg","path");return g.setAttribute("stroke",a),g.setAttribute("fill",d),h.push("fillColor","strokeColor"),t.strokeColor.contents.tag!=="NONE"&&(g.setAttribute("stroke-width",i.toString()),g.setAttribute("stroke-opacity",l.toString()),h.push("strokeColor","strokeWidth")),t.fillColor.contents.tag!=="NONE"&&(g.setAttribute("fill-opacity",u.toString()),h.push("fillColor")),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?g.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&g.setAttribute("stroke-dasharray",P.toString()),h.push("strokeDasharray","strokeStyle"),g.setAttribute("d",re(t.d.contents,o)),h.push("d"),p&&(g.setAttribute("marker-start",`url(#${n})`),h.push("startArrowhead")),k&&(g.setAttribute("marker-end",`url(#${e})`),h.push("endArrowhead")),s.appendChild(g),h.push(...M(t,s)),x(t,s,h),s},"RenderPath"),ae=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","polygon"),e=[];return e.push(...E(t,n)),e.push(...R(t,n)),e.push(...M(t,n)),e.push(...ft(t,n)),e.push(...mt(t,o,n)),x(t,n,e),n},"RenderPolygon"),ce=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","polyline"),e=[];return e.push(...E(t,n)),e.push(...R(t,n)),e.push(...M(t,n)),e.push(...ft(t,n)),e.push(...mt(t,o,n)),x(t,n,e),n},"RenderPolyline"),le=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","rect"),e=[];return e.push(...Wt(t,o,n)),e.push(...O(t,n)),e.push(...E(t,n)),e.push(...R(t,n)),e.push(...M(t,n)),e.push(...Xt(t,n)),e.push(...W(t,o,n)),x(t,n,e),n},"RenderRectangle"),de=c((t,{canvasSize:o,labels:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text"),r=[];r.push("x","y"),r.push(...E(t,e)),r.push(...R(t,e)),r.push(...M(t,e)),r.push(...Yt(t,e)),r.push(...W(t,o,e)),r.push(...Ht(t,e));const s=t.name,i=n.get(s.contents),a=t.center,[l,d]=C([a.contents[0],a.contents[1]],o);if(i&&i.tag==="TextData"){const u=i.descent.contents,h=i.height.contents,p=d+(h/2-u);e.setAttribute("x",l.toString()),e.setAttribute("y",p.toString()),r.push(...O(t,e))}else e.setAttribute("x",l.toString()),e.setAttribute("y",d.toString());return e.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),e.setAttribute("alignment-baseline",t.alignmentBaseline.contents),e.setAttribute("dominant-baseline",t.dominantBaseline.contents),e.setAttribute("ascent",t.ascent.contents.toString()),e.setAttribute("descent",t.descent.contents.toString()),e.setAttribute("text-anchor",t.textAnchor.contents.toString()),e.setAttribute("visibility",t.visibility.contents),r.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),x(t,e,r),e},"RenderText"),ot=c(({clientX:t,clientY:o},n)=>{const e=n.getScreenCTM();return e!==null?{x:(t-e.e)/e.a,y:(o-e.f)/e.d}:{x:0,y:0}},"getPosition"),ue=c(async(t,o,n,e)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","svg");r.setAttribute("xmlns","http://www.w3.org/2000/svg"),r.setAttribute("version","1.2"),r.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((a,l,d)=>{o(Pt(t))},"onDrag"),i=t.computeShapes(t.varyingValues);return await kt(i,r,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:e,texLabels:!1,pathResolver:n},{updateState:o,onDrag:s,parentSVG:r}),r},"RenderInteractive"),pe=c(async(t,o,n,e=!1)=>{const{varyingValues:r,computeShapes:s,labelCache:i,canvas:a,variation:l}=t,d=document.createElementNS("http://www.w3.org/2000/svg","svg");d.setAttribute("version","1.2"),d.setAttribute("xmlns","http://www.w3.org/2000/svg"),d.setAttribute("viewBox",`0 0 ${a.width} ${a.height}`);const u=s(r);return await kt(u,d,{labels:i,canvasSize:a.size,variation:l,namespace:n,texLabels:e,pathResolver:o},void 0),d},"RenderStatic"),he=c(async(t,o,n)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),r=H(t.shapes);for(const s of r){const i=await yt(s,o,n);e.appendChild(i)}return x(t,e,[...M(t,e),"shapes"]),e},"RenderGroup"),fe=c(async(t,o)=>{switch(t.shapeType){case"Circle":return Ut(t,o);case"Ellipse":return Jt(t,o);case"Equation":return Kt(t,o);case"Image":return ee(t,o);case"Line":return oe(t,o);case"Path":return ie(t,o);case"Polygon":return ae(t,o);case"Polyline":return ce(t,o);case"Rectangle":return le(t,o);case"Text":return de(t,o)}},"RenderShapeSvg"),yt=c(async(t,o,n)=>{if(t.shapeType==="Group")return await he(t,o,n);{const e=await fe(t,o);if(n){const r=document.createElementNS("http://www.w3.org/2000/svg","g");Nt(t)?r.setAttribute("pointer-events","visibleStroke"):Rt(t)?r.setAttribute("pointer-events","bounding-box"):r.setAttribute("pointer-events","auto"),r.appendChild(e);const s=c(i=>{const{clientX:a,clientY:l}=i,{x:d,y:u}=ot({clientX:a,clientY:l},n.parentSVG),{width:h,height:p,x:k,y:g}=i.target.getBBox({stroke:!0}),m=d-k,w=o.canvasSize[0]-h+(d-k),A=u-g,S=o.canvasSize[1]-p+(u-g);r.setAttribute("opacity","0.5");let f=0,y=0;const z=c(b=>{const{x:D,y:X}=ot(b,n.parentSVG),St=rt(D,m,w),vt=rt(X,A,S);f=St-d,y=u-vt,r.setAttribute("transform",`translate(${f},${-y})`)},"onMouseMove"),I=c(()=>{r.setAttribute("opacity","1"),document.removeEventListener("mouseup",I),document.removeEventListener("mousemove",z),n.onDrag(t.name.contents,f,y)},"onMouseUp");document.addEventListener("mouseup",I),document.addEventListener("mousemove",z)},"onMouseDown");return r.addEventListener("mousedown",s),r}else return e}},"RenderShape"),kt=c(async(t,o,n,e)=>{for(const r of t){const s=await yt(r,n,e);o.appendChild(s)}},"RenderShapes"),rt=c((t,o,n)=>Math.min(Math.max(t,o),n),"clamp"),ge=c(t=>{const o=zt(t.variation);return pt({...t,varyingValues:t.inputs.map(({meta:n})=>n.init.tag==="Sampled"?n.init.sampler(o):n.init.pending),currentStageIndex:0,params:U(t.varyingValues.length)})},"resample"),me=c((t,o)=>{const{constraintSets:n,optStages:e,currentStageIndex:r}=t,s=e[r],i=B(n.get(s),"missing stage"),a=new Float64Array(t.varyingValues);let l=0;const d=Bt((u,h,p)=>t.gradient(i,u,h,p).phi,a,t.params,()=>l++>=o);return{...t,varyingValues:Array.from(a),params:d}},"step"),wt=c((t,o=1e4)=>{const n=me(t,o);return q(n)&&!J(n)?At(n):n},"stepState"),At=c(t=>J(t)?t:{...t,currentStageIndex:t.currentStageIndex+1,params:U(t.varyingValues.length)},"nextStage"),ye=c((t,o=1e4)=>{let n=t;for(;n.params.optStatus!=="Error"&&(!q(n)||!J(n));)q(n)&&(n=At(n)),n=wt(n,o);return n.params.optStatus==="Error"?$({errorType:"RuntimeError",...It("",n)}):_(n)},"stepUntilConvergence"),ke=c(async t=>{const o=Dt(t.domain),n=Vt(r=>jt(t.substance,r),o);return n.isErr()?$(n.error):await Ot(t.variation,t.style,...n.value)},"compileTrio"),we=c(async t=>{const o=Gt(),n=await dt(t.shapes,o);if(n.isErr())throw Error(at(n.error));return pt({...t,labelCache:n.value})},"prepareState"),q=c(t=>t.params.optStatus==="EPConverged","stateConverged"),J=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function st(t){const o=await fetch(t);if(!o.ok){console.error(`could not fetch ${t}`);return}return await o.text()}c(st,"fetchResolver");class Y extends Z.Component{canvasRef=Z.createRef();penroseState=void 0;timerID=void 0;constructor(o){super(o),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const o=await ke(this.props);o.isOk()?(this.penroseState=await we(o.value),this.setState({error:void 0})):this.setState({error:o.error})};converge=async()=>{if(this.penroseState){const o=ye(this.penroseState);o.isOk()?this.penroseState=o.value:this.setState({error:o.error})}};tick=()=>{this.props.animate&&this.penroseState&&!q(this.penroseState)&&(this.penroseState=wt(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async o=>{if(this.props.domain!==o.domain||this.props.substance!==o.substance||this.props.style!==o.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==o.variation||this.props.animate!==o.animate){this.penroseState.variation=this.props.variation,this.penroseState=ge(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==o.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return L("div",{children:"rendering..."});{const o=this.canvasRef.current;if(this.penroseState){const n=await(this.props.interactive===!1?pe(this.penroseState,this.props.imageResolver??st,this.props.name??""):ue(this.penroseState,async e=>{this.penroseState=e,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??st,this.props.name??""));o.firstChild!==null?o.replaceChild(n,o.firstChild):o.appendChild(n),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return L("div",{children:"rendering..."})}};render=()=>{const{error:o}=this.state;return Q("div",{style:{width:"100%",height:"100%"},children:[!o&&L("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),o&&Q("div",{style:{padding:"1em",height:"100%"},children:[L("div",{style:{fontWeight:700},children:"1 error:"}),L("div",{style:{fontFamily:"monospace"},children:at(o).toString().split(`
`).map((n,e)=>L("p",{style:{margin:0},children:n},`err-ln-${e}`))})]})]})}}c(Y,"Simple");try{Y.displayName="Simple",Y.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{Y as S};
//# sourceMappingURL=Simple-6fe80b4b.js.map
