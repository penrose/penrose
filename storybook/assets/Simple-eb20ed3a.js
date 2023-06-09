var Mt=Object.defineProperty;var c=(t,o)=>Mt(t,"name",{value:o,configurable:!0});import{b as Ct,R as Et,T as $t,A as zt,s as Rt,m as Tt,g as v,e as T,a as pt,o as W,d as q,i as ut,t as H,f as U,h as x,j as X,r as tt,_ as Lt,k as K,n as Nt,p as et,q as It,u as Vt,v as nt,w as Dt,x as jt,y as Bt,B as Ft,C as Gt,D as Ot,E as qt,F as Xt,c as _t,G as Yt,H as Wt,I as Ht,J as ht,K as Ut}from"./svg-a2d3d679.js";import{R as ot,j as I,a as rt}from"./jsx-runtime-6d42fd9d.js";const Pt=c(()=>{const t=Ct();Et(t);const o=new $t({packages:zt,macros:{textsc:["\\style{font-variant-caps: small-caps}{\\text{#1}}",1]},inlineMath:[["$","$"],["\\(","\\)"]],processEscapes:!0,formatError:(s,i)=>{throw Error(i.message)}}),n=new Rt.SVG({fontCache:"none"}),e=Tt.mathjax.document("",{InputJax:o,OutputJax:n});return c(s=>{try{const i=e.convert(s,{});return W(i.firstChild)}catch(i){return T(i.message)}},"convert")},"mathjaxInit"),Jt=c(t=>{const o=/^(\d+(?:\.\d+)?)\s*(px|in|cm|mm)$/,n=t.match(o);if(!n)return;const e=parseFloat(n[1]),r=n[2];return{number:e,unit:r}},"parseFontSize"),Kt=c((t,o)=>({px:1,in:96,cm:37.79527559055118,mm:3.7795275590551185})[o]*t,"toPxFontSize"),Zt=c(async(t,o)=>new Promise(n=>{const e=v(t.string,""),r=v(t.fontSize,"");(r===""||e==="")&&n(T(`Label 'string' and 'fontSize' must be non-empty and non-optimized for ${t.name.contents}`));const s=o(e);if(s.isErr()){n(T(`MathJax could not render $${e}$: ${s.error}`));return}const i=s.value,a=i.getAttribute("viewBox");if(a===null){n(T(`No ViewBox found for MathJax output $${e}$`));return}const l=a.split(" "),p=parseFloat(l[2])/1e3,d=parseFloat(l[3])/1e3,u=-parseFloat(i.style.verticalAlign),h=parseFloat(i.getAttribute("height")),f=Jt(r);if(f){const{number:g,unit:y}=f,A=c($=>$*Kt(g,y),"em_to_px"),k=A(p),S=A(d),m=u/h*S,E=S-m;n(W({body:i,width:k,height:S,descent:m,ascent:E}))}else{n(T('Invalid font size format. Only "px", "in", "cm", and "mm" units are supported.'));return}}),"tex2svg"),L=c(t=>({tag:"FloatV",contents:t}),"floatV"),st=c((t,o,n,e)=>({tag:"TextData",width:L(t),height:L(o),descent:L(n),ascent:L(e)}),"textData"),Qt=c((t,o,n,e,r)=>({tag:"EquationData",width:L(t),height:L(o),ascent:L(n),descent:L(e),rendered:r}),"equationData"),ft=c(t=>{const o=v(t.fontFamily),n=v(t.fontSize),e=v(t.fontStretch),r=v(t.fontStyle),s=v(t.fontVariant),i=v(t.fontWeight),a=v(t.lineHeight),l=`${e} ${r} ${s} ${i} ${n} ${o}`;return a!==""?l.concat(`/${a}`):l},"toFontRule"),mt=c(async(t,o)=>{const n=new Map;for(const e of t)if(e.shapeType==="Equation"){const r=v(e.name),s=await Zt(e,o);if(s.isErr())return T({errorType:"SubstanceError",tag:"Fatal",message:s.error});const{body:i,width:a,height:l,ascent:p,descent:d}=s.value,u=Qt(a===1/0?0:a,l===1/0?0:l,p,d,i);n.set(r,u)}else if(e.shapeType==="Text"){const r=v(e.name);let s;const i=te(v(e.string),ft(e));i.width&&i.height?s=st(i.width,i.height,i.actualDescent,i.actualAscent):s=st(0,0,0,0),n.set(r,s)}else if(e.shapeType==="Group"){const r=pt(e.shapes),s=await mt(r,o);if(s.isErr())return s;for(const[i,a]of s.value.entries())n.set(i,a)}return W(n)},"collectLabels");function te(t,o){const n=document.createElement("canvas"),e=n.getContext("2d");e.textBaseline="alphabetic",e.font=o;const r=e.measureText(t);return n.remove(),{width:Math.abs(r.actualBoundingBoxLeft)+Math.abs(r.actualBoundingBoxRight),height:Math.abs(r.actualBoundingBoxAscent)+Math.abs(r.actualBoundingBoxDescent),actualDescent:Math.abs(r.actualBoundingBoxDescent),actualAscent:Math.abs(r.actualBoundingBoxAscent)}}c(te,"measureText");const R=c((t,o,n,e)=>{if(typeof n.contents!="number"&&n.contents.tag==="Input"){const{index:r,meta:s}=q(o.get(n.contents),"missing input");s.init.tag==="Pending"&&(t[r]=e.contents)}},"setPendingProperty"),gt=c((t,o,n,e)=>{for(const r of t)if(r.shapeType==="Group"){const s=pt(r.shapes);gt(s,o,n,e)}else if(r.shapeType==="Equation"){const s=q(n.get(r.name.contents),"missing label");if(s.tag!=="EquationData")throw Error(`for ${r.shapeType} ${r.name.contents} got unexpected ${s.tag}`);R(o,e,r.width,s.width),R(o,e,r.height,s.height),R(o,e,r.ascent,s.ascent),R(o,e,r.descent,s.descent)}else if(r.shapeType==="Text"){const s=q(n.get(r.name.contents),"missing label");if(s.tag!=="TextData")throw Error(`for ${r.shapeType} ${r.name.contents} got unexpected ${s.tag}`);R(o,e,r.width,s.width),R(o,e,r.height,s.height),R(o,e,r.ascent,s.ascent),R(o,e,r.descent,s.descent)}},"insertPendingHelper"),yt=c(t=>{const o=[...t.varyingValues],n=new Map(t.inputs.map(({handle:e,meta:r},s)=>[e,{index:s,meta:r}]));return gt(t.shapes,o,t.labelCache,n),{...t,varyingValues:o}},"insertPending"),it={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},M=c((t,o,n)=>{const e=["strokeStyle","name","ensureOnCanvas"],r=new Set(n.concat(e));for(const[s,i]of t.passthrough)if(!(i.tag==="StrV"&&i.contents===""||r.has(s)))if(ut(s,it)){const a=it[s];o.hasAttribute(a)||o.setAttribute(a,i.contents.toString())}else if(s==="style"&&i.contents!==""){const a=o.getAttribute(s);a===null?o.setAttribute(s,i.contents.toString()):o.setAttribute(s,`${a}${i.contents.toString()}`)}else o.hasAttribute(s)||o.setAttribute(s,i.contents.toString())},"attrAutoFillSvg"),z=c((t,o)=>{const n=t.fillColor,e=H(n.contents);return o.setAttribute("fill",U(n.contents)),n.contents.tag!=="NONE"&&o.setAttribute("fill-opacity",e.toString()),["fillColor"]},"attrFill"),wt=c((t,o,n)=>{const e=t.center,[r,s]=x([e.contents[0],e.contents[1]],o);return n.setAttribute("cx",r.toString()),n.setAttribute("cy",s.toString()),["center"]},"attrCenter"),kt=c((t,o)=>{let n=t.scale.contents;n=n||1;let e=o.getAttribute("transform");return e=e===null?`scale(${n})`:e+`scale{${n}}`,o.setAttribute("transform",e),["scale"]},"attrScale"),St=c((t,o,n)=>{const e=t.center,[r,s]=x([e.contents[0],e.contents[1]],o),i=t.width,a=t.height;let l=n.getAttribute("transform");return l=l===null?`translate(${r-i.contents/2}, ${s-a.contents/2})`:l+`translate(${r-i.contents/2}, ${s-a.contents/2})`,n.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),ee=c((t,o,n)=>{const e=t.center,[r,s]=x([e.contents[0],e.contents[1]],o),i=t.width,a=t.height;return n.setAttribute("x",(r-i.contents/2).toString()),n.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),P=c((t,o,n)=>{t.width,t.height;const e=t.center,r=t.rotation.contents,[s,i]=x([e.contents[0],e.contents[1]],o);let a=n.getAttribute("transform");return a=a===null?`rotate(${r}, ${s}, ${i})`:a+`rotate(${r}, ${s}, ${i})`,n.setAttribute("transform",a),["rotation","center","width","height"]},"attrRotation"),D=c((t,o)=>{const n=t.width,e=t.height;return o.setAttribute("width",n.contents.toString()),o.setAttribute("height",e.contents.toString()),["width","height"]},"attrWH"),ne=c((t,o)=>{const n=t.cornerRadius;return o.setAttribute("rx",n.contents.toString()),["cornerRadius"]},"attrCornerRadius"),oe=c((t,o)=>{const n=t.string,e=document.createTextNode(n.contents.toString());return o.appendChild(e),["string"]},"attrString"),re="7,5",N=c((t,o)=>{const n=[],e=t.strokeColor,r=H(e.contents),s=t.strokeWidth.contents;return o.setAttribute("stroke",U(e.contents)),n.push("strokeColor","strokeWidth"),e.contents.tag!=="NONE"&&(o.setAttribute("stroke-opacity",r.toString()),o.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?(o.setAttribute("stroke-dasharray",t.strokeDasharray.contents),n.push("strokeDasharray")):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(o.setAttribute("stroke-dasharray",re.toString()),n.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""&&(o.setAttribute("stroke-linecap",t.strokeLinecap.contents),n.push("strokeLinecap"))),n},"attrStroke"),C=c((t,o)=>{const n=t.name,e=document.createElementNS("http://www.w3.org/2000/svg","title");return e.textContent=n.contents,o.appendChild(e),["name"]},"attrTitle"),se=c((t,o)=>{const n=ft(t),e=o.getAttribute("style");return o.setAttribute("style",e?`${e}; font: ${n};`:`font: ${n};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),At=c((t,o,n)=>{const r=t.points.contents.map(s=>x([s[0],s[1]],o));return n.setAttribute("points",r.toString()),["points"]},"attrPolyPoints"),ie=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","circle"),e=[];return e.push(...z(t,n)),e.push(...wt(t,o,n)),e.push(...N(t,n)),e.push(...C(t,n)),n.setAttribute("r",t.r.contents.toString()),e.push("r"),M(t,n,e),n},"RenderCircle"),ae=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),e=[];return e.push(...z(t,n)),e.push(...wt(t,o,n)),e.push(...N(t,n)),e.push(...C(t,n)),n.setAttribute("rx",t.rx.contents.toString()),e.push("rx"),n.setAttribute("ry",t.ry.contents.toString()),e.push("ry"),M(t,n,e),n},"RenderEllipse"),at=c((t,[o,n],e)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","text");return r.textContent=t,z(e,r),D(e,r),r.setAttribute("x",`${o}`),r.setAttribute("y",`${n}`),r.setAttribute("alignment-baseline","alphabetic"),r.setAttribute("dominant-baseline","alphabetic"),r.setAttribute("text-anchor","middle"),r},"placeholderString"),ce=c((t,o)=>{const{canvasSize:n,labels:e,texLabels:r}=o,{center:s}=t,[i,a]=x([s.contents[0],s.contents[1]],n);if(r){const u=a+t.height.contents/2-t.descent.contents;return at(`$${v(t.string)}$`,[i,u],t)}const l=document.createElementNS("http://www.w3.org/2000/svg","g"),p=[];p.push(...P(t,n,l)),p.push(...St(t,n,l)),p.push(...C(t,l));const d=e.get(v(t.name));if(d&&d.tag==="EquationData"){const u=d.rendered.cloneNode(!0),h=u.getElementsByTagName("g")[0];p.push(...z(t,h)),p.push(...D(t,u)),h.setAttribute("stroke","none"),h.setAttribute("stroke-width","0");const f=t.fontSize;return u.setAttribute("style",`font-size: ${f.contents}`),l.appendChild(u),M(t,l,p),l}else return at(v(t.string),[i,a],t)},"RenderEquation"),le=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let de=1;const pe=c((t,o)=>{const n="--inject-",e={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},r=n+de++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const l=o?new Set:void 0,p=new Set,d=[];let u=!1,h,f;if(i.length){for(h=0;h<i.length;h++){const w=i[h].localName;ut(w,e)&&p.add(w)}p.forEach(w=>{(e[w]||[w]).forEach(function(m){d.indexOf(m)<0&&d.push(m)})}),d.length&&d.push("style");const g=t.getElementsByTagName("*");let y=t,A,k,S;for(h=-1;y!==null;){if(y.localName==="style")k=y.textContent,S=k&&k.replace(s,function(w,m){return l&&l.add(m),"url(#"+m+r+")"}),S!==k&&(y.textContent=S);else if(y.hasAttributes()){for(f=0;f<d.length;f++)A=d[f],k=y.getAttribute(A),S=k&&k.replace(s,function(w,m){return l&&l.add(m),"url(#"+m+r+")"}),S&&S!==k&&y.setAttribute(A,S);for(const w of["xlink:href","href"]){let m=y.getAttribute(w);m&&/^\s*#/.test(m)&&(m=m.trim(),y.setAttribute(w,m+r),l&&l.add(m.substring(1)))}}y=g.item(++h)}for(h=0;h<i.length;h++)a=i[h],(!l||l.has(a.id))&&(a.id+=r,u=!0)}return u},"makeIdsUnique"),ue=c(async(t,{canvasSize:o,pathResolver:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),r=[],s=t.href.contents;let i=await n(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=le),r.push("href"),e.innerHTML=i;const a=e.querySelector("svg");return pe(e,!1),r.push(...D(t,a)),r.push(...P(t,o,e)),r.push(...St(t,o,e)),r.push(...C(t,e)),M(t,e,r),e},"RenderImage"),_=c((t,o,n,e,r,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",tt(e.width*r).toString()),i.setAttribute("markerHeight",tt(e.height*r).toString()),i.setAttribute("viewBox",e.viewbox),i.setAttribute("refX",e.refX.toString()),i.setAttribute("refY",e.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",e.path),e.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",o),i.setAttribute("stroke-opacity",n.toString())):(a.setAttribute("fill",o),a.setAttribute("fill-opacity",n.toString())),e.style&&Object.entries(e.style).forEach(([l,p])=>{a.setAttribute(l,p)}),i.appendChild(a),i},"arrowHead"),he=c((t,o,n)=>{const e=[],[r,s]=[t.start.contents[0],t.start.contents[1]],[i,a]=[t.end.contents[0],t.end.contents[1]],l=t.startArrowheadSize.contents,p=t.endArrowheadSize.contents,d=t.strokeWidth.contents;e.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const u=Math.sqrt((r-i)**2+(s-a)**2);let h,f;if(o){const k=(t.flipStartArrowhead.contents?o.refX:o.width-o.refX)*l*d,S=k/u*(r-i),w=k/u*(s-a);[h,f]=[r-S,s-w]}else[h,f]=[r,s];let g,y;if(n){const A=(n.width-n.refX)*p*d;[g,y]=[i-A/u*(i-r),a-A/u*(a-s)]}else[g,y]=[i,a];return[[[h,f],[g,y]],e]},"makeRoomForArrows"),fe=c((t,{canvasSize:o,namespace:n,variation:e})=>{const r=X(t.startArrowhead.contents),s=X(t.endArrowhead.contents),[[[i,a],[l,p]],d]=he(t,r,s),u=U(t.strokeColor.contents),h=H(t.strokeColor.contents),f=document.createElementNS("http://www.w3.org/2000/svg","g"),g=document.createElementNS("http://www.w3.org/2000/svg","line"),[y,A]=x([i,a],o),[k,S]=x([l,p],o);g.setAttribute("x1",y.toString()),g.setAttribute("y1",A.toString()),g.setAttribute("x2",k.toString()),g.setAttribute("y2",S.toString());const w=`${n}-${e}-${t.name.contents}`,m=w+"-startArrowId",E=w+"-endArrowId";if(r){const $=t.startArrowheadSize.contents,V=t.flipStartArrowhead.contents;f.appendChild(_(m,u,h,r,$,V))}if(s){const $=t.endArrowheadSize.contents;f.appendChild(_(E,u,h,s,$,!1))}return d.push("startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize"),d.push(...N(t,g)),r&&(g.setAttribute("marker-start",`url(#${m})`),d.push("startArrowhead")),s&&(g.setAttribute("marker-end",`url(#${E})`),d.push("endArrowhead")),f.appendChild(g),d.push(...C(t,f)),M(t,f,d),f},"RenderLine"),me=c((t,o)=>t.map(n=>{const{cmd:e,contents:r}=n;if(r.length===0&&e!=="Z")return console.error("WARNING: empty path"),"";const s=Lt.flatten(r.map(i=>{switch(i.tag){case"CoordV":return x([i.contents[0],i.contents[1]],o);case"ValueV":return i.contents}})).join(" ");return`${e} ${s}`}).join(" "),"toPathString"),ge=c(t=>{const o=document.createElementNS("http://www.w3.org/2000/svg","filter");return o.setAttribute("id",t),o.setAttribute("x","0"),o.setAttribute("y","0"),o.setAttribute("width","200%"),o.setAttribute("height","200%"),o.innerHTML=`
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
    `,o},"Shadow"),ye=c((t,{canvasSize:o})=>{const n=t.name.contents+"-startArrowId",e=t.name.contents+"-endArrowId",r=t.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=U(t.strokeColor.contents),a=H(t.strokeColor.contents),l=[],p=X(t.startArrowhead.contents),d=X(t.endArrowhead.contents);if(p){const h=t.name.contents+"-startArrowId",f=t.startArrowheadSize.contents,g=t.flipStartArrowhead.contents;s.appendChild(_(h,i,a,p,f,g))}if(d){const h=t.name.contents+"-endArrowId",f=t.endArrowheadSize.contents;s.appendChild(_(h,i,a,d,f,!1))}l.push("name","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(ge(r));const u=document.createElementNS("http://www.w3.org/2000/svg","path");return l.push(...z(t,u)),l.push(...N(t,u)),u.setAttribute("d",me(t.d.contents,o)),l.push("d"),p&&(u.setAttribute("marker-start",`url(#${n})`),l.push("startArrowhead")),d&&(u.setAttribute("marker-end",`url(#${e})`),l.push("endArrowhead")),s.appendChild(u),l.push(...C(t,s)),M(t,s,l),s},"RenderPath"),we=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","polygon"),e=[];return e.push(...z(t,n)),e.push(...N(t,n)),e.push(...C(t,n)),e.push(...kt(t,n)),e.push(...At(t,o,n)),M(t,n,e),n},"RenderPolygon"),ke=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","polyline"),e=[];return e.push(...z(t,n)),e.push(...N(t,n)),e.push(...C(t,n)),e.push(...kt(t,n)),e.push(...At(t,o,n)),M(t,n,e),n},"RenderPolyline"),Se=c((t,{canvasSize:o})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","rect"),e=[];return e.push(...ee(t,o,n)),e.push(...D(t,n)),e.push(...z(t,n)),e.push(...N(t,n)),e.push(...C(t,n)),e.push(...ne(t,n)),e.push(...P(t,o,n)),M(t,n,e),n},"RenderRectangle"),Ae=c((t,{canvasSize:o,labels:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text"),r=[];r.push("x","y"),r.push(...z(t,e)),r.push(...N(t,e)),r.push(...C(t,e)),r.push(...oe(t,e)),r.push(...P(t,o,e)),r.push(...se(t,e));const s=t.name,i=n.get(s.contents),a=t.center,[l,p]=x([a.contents[0],a.contents[1]],o);if(i&&i.tag==="TextData"){const d=i.descent.contents,u=i.height.contents,h=p+(u/2-d);e.setAttribute("x",l.toString()),e.setAttribute("y",h.toString()),r.push(...D(t,e))}else e.setAttribute("x",l.toString()),e.setAttribute("y",p.toString());return e.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),e.setAttribute("alignment-baseline",t.alignmentBaseline.contents),e.setAttribute("dominant-baseline",t.dominantBaseline.contents),e.setAttribute("ascent",t.ascent.contents.toString()),e.setAttribute("descent",t.descent.contents.toString()),e.setAttribute("text-anchor",t.textAnchor.contents.toString()),e.setAttribute("visibility",t.visibility.contents),r.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),M(t,e,r),e},"RenderText"),ve=c((t,o,n,e)=>{const r=[...t.varyingValues];return{...t,params:K(r.length),varyingValues:r}},"dragUpdate"),ct=c(({clientX:t,clientY:o},n)=>{const e=n.getScreenCTM();return e!==null?{x:(t-e.e)/e.a,y:(o-e.f)/e.d}:{x:0,y:0}},"getPosition"),xe=c(async(t,o,n,e)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","svg");r.setAttribute("xmlns","http://www.w3.org/2000/svg"),r.setAttribute("version","1.2"),r.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((a,l,p)=>{o(ve(t))},"onDrag"),i=t.computeShapes(t.varyingValues);return await vt(i,r,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:e,texLabels:!1,pathResolver:n},{updateState:o,onDrag:s,parentSVG:r}),r},"RenderInteractive"),be=c(async(t,o,n,e=!1)=>{const{varyingValues:r,computeShapes:s,labelCache:i,canvas:a,variation:l}=t,p=document.createElementNS("http://www.w3.org/2000/svg","svg");p.setAttribute("version","1.2"),p.setAttribute("xmlns","http://www.w3.org/2000/svg"),p.setAttribute("viewBox",`0 0 ${a.width} ${a.height}`);const d=s(r),u=d.map(b=>Nt(b)),h=et(u.map(b=>It(b))),f=et(u.map(b=>Vt(b))),g=nt(u.map(b=>Dt(b))),y=nt(u.map(b=>jt(b))),A=[h,f,g,y],[k,S,w,m]=(await Bt(Ft(A)))(b=>b.val).secondary,[E,$]=x([k,S],[a.width,a.height]),[V,j]=x([w,m],[a.width,a.height]),B=[E,j],F=[V-E,$-j];p.setAttribute("penrose","0");const G=document.createElementNS("https://penrose.cs.cmu.edu/metadata","penrose"),Q=document.createElementNS("https://penrose.cs.cmu.edu/croppedViewBox","croppedViewBox");return Q.insertAdjacentText("afterbegin",`${B[0]} ${B[1]} ${F[0]} ${F[1]}`),G.appendChild(Q),p.appendChild(G),await vt(d,p,{labels:i,canvasSize:a.size,variation:l,namespace:n,texLabels:e,pathResolver:o},void 0),p},"RenderStatic"),Me=c(async(t,o,n)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),r=t.clipPath.contents;let s,i;if(r.tag==="Clip"){const l=r.contents;s=l.name.contents;const p=await O(l,o,n),d=document.createElementNS("http://www.w3.org/2000/svg","clipPath");i=o.namespace+s+"-clip",d.setAttribute("id",i),d.appendChild(p),e.appendChild(d)}const a=t.shapes.contents;for(const l of a){const p=l.name.contents;if(r.tag==="Clip"){if(p!==s){const d=await O(l,o,n);d.setAttribute("clip-path",`url(#${i})`),e.appendChild(d)}}else{const d=await O(l,o,n);e.appendChild(d)}}return M(t,e,[...C(t,e),"shapes","clipPath"]),e},"RenderGroup"),Ce=c(async(t,o)=>{switch(t.shapeType){case"Circle":return ie(t,o);case"Ellipse":return ae(t,o);case"Equation":return ce(t,o);case"Image":return ue(t,o);case"Line":return fe(t,o);case"Path":return ye(t,o);case"Polygon":return we(t,o);case"Polyline":return ke(t,o);case"Rectangle":return Se(t,o);case"Text":return Ae(t,o)}},"RenderShapeSvg"),O=c(async(t,o,n)=>{if(t.shapeType==="Group")return await Me(t,o,n);{const e=await Ce(t,o);if(n){const r=document.createElementNS("http://www.w3.org/2000/svg","g");Gt(t)?r.setAttribute("pointer-events","visibleStroke"):Ot(t)?r.setAttribute("pointer-events","bounding-box"):r.setAttribute("pointer-events","auto"),r.appendChild(e);const s=c(i=>{const{clientX:a,clientY:l}=i,{x:p,y:d}=ct({clientX:a,clientY:l},n.parentSVG),{width:u,height:h,x:f,y:g}=i.target.getBBox({stroke:!0}),y=p-f,A=o.canvasSize[0]-u+(p-f),k=d-g,S=o.canvasSize[1]-h+(d-g);r.setAttribute("opacity","0.5");let w=0,m=0;const E=c(V=>{const{x:j,y:B}=ct(V,n.parentSVG),F=lt(j,y,A),G=lt(B,k,S);w=F-p,m=d-G,r.setAttribute("transform",`translate(${w},${-m})`)},"onMouseMove"),$=c(()=>{r.setAttribute("opacity","1"),document.removeEventListener("mouseup",$),document.removeEventListener("mousemove",E),n.onDrag(t.name.contents,w,m)},"onMouseUp");document.addEventListener("mouseup",$),document.addEventListener("mousemove",E)},"onMouseDown");return r.addEventListener("mousedown",s),r}else return e}},"RenderShape"),vt=c(async(t,o,n,e)=>{for(const r of t){const s=await O(r,n,e);o.appendChild(s)}},"RenderShapes"),lt=c((t,o,n)=>Math.min(Math.max(t,o),n),"clamp"),Ee=c(t=>{const o=qt(t.variation);return yt({...t,varyingValues:t.inputs.map(({meta:n})=>n.init.tag==="Sampled"?n.init.sampler(o):n.init.pending),currentStageIndex:0,params:K(t.varyingValues.length)})},"resample"),$e=c((t,o)=>{const{constraintSets:n,optStages:e,currentStageIndex:r}=t,s=e[r],i=q(n.get(s),"missing stage"),a=new Float64Array(t.varyingValues);let l=0;const p=Ut((d,u,h)=>t.gradient(i,d,u,h).phi,a,t.params,()=>l++>=o);return{...t,varyingValues:Array.from(a),params:p}},"step"),xt=c((t,o=1e4)=>{const n=$e(t,o);return Y(n)&&!Z(n)?bt(n):n},"stepState"),bt=c(t=>Z(t)?t:{...t,currentStageIndex:t.currentStageIndex+1,params:K(t.varyingValues.length)},"nextStage"),ze=c((t,o=1e4)=>{let n=t;for(;n.params.optStatus!=="Error"&&(!Y(n)||!Z(n));)Y(n)&&(n=bt(n)),n=xt(n,o);return n.params.optStatus==="Error"?T({errorType:"RuntimeError",...Xt("",n)}):W(n)},"stepUntilConvergence"),Re=c(async t=>{const o=_t(t.domain),n=Yt(r=>Wt(t.substance,r),o);return n.isErr()?T(n.error):await Ht(t.variation,t.style,...n.value)},"compileTrio"),Te=c(async t=>{const o=Pt(),n=await mt(t.shapes,o);if(n.isErr())throw Error(ht(n.error));return yt({...t,labelCache:n.value})},"prepareState"),Y=c(t=>t.params.optStatus==="EPConverged","stateConverged"),Z=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function dt(t){const o=await fetch(t);if(!o.ok){console.error(`could not fetch ${t}`);return}return await o.text()}c(dt,"fetchResolver");class J extends ot.Component{canvasRef=ot.createRef();penroseState=void 0;timerID=void 0;constructor(o){super(o),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const o=await Re(this.props);o.isOk()?(this.penroseState=await Te(o.value),this.setState({error:void 0})):this.setState({error:o.error})};converge=async()=>{if(this.penroseState){const o=ze(this.penroseState);o.isOk()?this.penroseState=o.value:this.setState({error:o.error})}};tick=()=>{this.props.animate&&this.penroseState&&!Y(this.penroseState)&&(this.penroseState=xt(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async o=>{if(this.props.domain!==o.domain||this.props.substance!==o.substance||this.props.style!==o.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==o.variation||this.props.animate!==o.animate){this.penroseState.variation=this.props.variation,this.penroseState=Ee(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==o.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return I("div",{children:"rendering..."});{const o=this.canvasRef.current;if(this.penroseState){const n=await(this.props.interactive===!1?be(this.penroseState,this.props.imageResolver??dt,this.props.name??""):xe(this.penroseState,async e=>{this.penroseState=e,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??dt,this.props.name??""));o.firstChild!==null?o.replaceChild(n,o.firstChild):o.appendChild(n),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return I("div",{children:"rendering..."})}};render=()=>{const{error:o}=this.state;return rt("div",{style:{width:"100%",height:"100%"},children:[!o&&I("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),o&&rt("div",{style:{padding:"1em",height:"100%"},children:[I("div",{style:{fontWeight:700},children:"1 error:"}),I("div",{style:{fontFamily:"monospace"},children:ht(o).toString().split(`
`).map((n,e)=>I("p",{style:{margin:0},children:n},`err-ln-${e}`))})]})]})}}c(J,"Simple");try{J.displayName="Simple",J.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{J as S};
//# sourceMappingURL=Simple-eb20ed3a.js.map
