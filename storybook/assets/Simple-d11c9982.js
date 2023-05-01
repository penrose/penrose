var ft=Object.defineProperty;var c=(t,o)=>ft(t,"name",{value:o,configurable:!0});import{t as j,b as I,c as C,d as ht,g as F,e as _,f as T,r as Y,l as gt,i as mt,h as yt,j as kt,s as wt,k as Q,m as tt,n as et,o as At,p as St,q as bt,u as vt,w as xt,x as Mt,y as Ct,z as ot}from"./PenrosePrograms-6c7be2fc.js";import{R as B}from"./index-74c5fbfa.js";import{j as L,a as U}from"./jsx-runtime-9c5bc5e6.js";const P={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},x=c((t,o,r)=>{const e=["strokeStyle","name","ensureOnCanvas"],n=new Set(r.concat(e));for(const[i,s]of t.passthrough)if(!(s.tag==="StrV"&&s.contents===""||n.has(i)))if(i in P){const a=P[i];if(!o.hasAttribute(a))o.setAttribute(a,s.contents.toString());else if(i==="style"){const l=o.getAttribute(i);l===null?o.setAttribute(i,s.contents.toString()):o.setAttribute(i,`${l}${s.contents.toString()}`)}else o.hasAttribute(i)||o.setAttribute(i,s.contents.toString())}else o.setAttribute(i,s.contents.toString())},"attrAutoFillSvg"),E=c((t,o)=>{const r=t.fillColor,e=j(r.contents);return o.setAttribute("fill",I(r.contents)),r.contents.tag!=="NONE"&&o.setAttribute("fill-opacity",e.toString()),["fillColor"]},"attrFill"),rt=c((t,o,r)=>{const e=t.center,[n,i]=C([e.contents[0],e.contents[1]],o);return r.setAttribute("cx",n.toString()),r.setAttribute("cy",i.toString()),["center"]},"attrCenter"),nt=c((t,o)=>{let r=t.scale.contents;r=r||1;let e=o.getAttribute("transform");return e=e===null?`scale(${r})`:e+`scale{${r}}`,o.setAttribute("transform",e),["scale"]},"attrScale"),st=c((t,o,r)=>{const e=t.center,[n,i]=C([e.contents[0],e.contents[1]],o),s=t.width,a=t.height;let l=r.getAttribute("transform");return l=l===null?`translate(${n-s.contents/2}, ${i-a.contents/2})`:l+`translate(${n-s.contents/2}, ${i-a.contents/2})`,r.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),Et=c((t,o,r)=>{const e=t.center,[n,i]=C([e.contents[0],e.contents[1]],o),s=t.width,a=t.height;return r.setAttribute("x",(n-s.contents/2).toString()),r.setAttribute("y",(i-a.contents/2).toString()),["center","width","height"]},"attrXY"),V=c((t,o,r)=>{const e=t.width,n=t.height,i=t.center,s=t.rotation.contents,[a,l]=C([i.contents[0],i.contents[1]],o);let d=r.getAttribute("transform");return d=d===null?`rotate(${s}, ${a-e.contents/2}, ${l-n.contents/2})`:d+`rotate(${s}, ${a-e.contents/2}, ${l-n.contents/2})`,r.setAttribute("transform",d),["rotation","center","width","height"]},"attrRotation"),O=c((t,o)=>{const r=t.width,e=t.height;return o.setAttribute("width",r.contents.toString()),o.setAttribute("height",e.contents.toString()),["width","height"]},"attrWH"),Lt=c((t,o)=>{const r=t.cornerRadius;return o.setAttribute("rx",r.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Rt=c((t,o)=>{const r=t.string,e=document.createTextNode(r.contents.toString());return o.appendChild(e),["string"]},"attrString"),W="7,5",R=c((t,o)=>{const r=[],e=t.strokeColor,n=j(e.contents),i=t.strokeWidth.contents;return o.setAttribute("stroke",I(e.contents)),r.push("strokeColor","strokeWidth"),e.contents.tag!=="NONE"&&(o.setAttribute("stroke-opacity",n.toString()),o.setAttribute("stroke-width",i.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?o.setAttribute("stroke-dasharray",t.strokeDasharray.contents):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(o.setAttribute("stroke-dasharray",W.toString()),r.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""?o.setAttribute("stroke-linecap",t.strokeLinecap.contents):o.setAttribute("stroke-linecap","butt"),r.push("strokeLinecap")),r},"attrStroke"),M=c((t,o)=>{const r=t.name,e=document.createElementNS("http://www.w3.org/2000/svg","title");return e.textContent=r.contents,o.appendChild(e),["name"]},"attrTitle"),Nt=c((t,o)=>{const r=ht(t),e=o.getAttribute("style");return o.setAttribute("style",e?`${e}; font: ${r};`:`font: ${r};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),it=c((t,o,r)=>{const n=t.points.contents.map(i=>C([i[0],i[1]],o));return r.setAttribute("points",n.toString()),["points"]},"attrPolyPoints"),zt=c((t,{canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","circle"),e=[];return e.push(...E(t,r)),e.push(...rt(t,o,r)),e.push(...R(t,r)),e.push(...M(t,r)),r.setAttribute("r",t.r.contents.toString()),e.push("r"),x(t,r,e),r},"RenderCircle"),jt=c((t,o,r,e)=>{const n=[...t.varyingValues],{constraintSets:i,optStages:s}=t,{inputMask:a,objMask:l,constrMask:d}=i.get(s[0]),h=[...a];for(const y of t.shapes)if(y.name.contents===o)for(const u of It(y,[r,e],n))h[u]=!1;return Object.assign(Object.assign({},t),{params:F(h,l,d),varyingValues:n})},"dragUpdate"),It=c((t,o,r)=>{switch(t.shapeType){case"Path":return console.log("Path drag unimplemented",t),[];case"Polygon":return console.log("Polygon drag unimplemented",t),[];case"Polyline":return console.log("Polyline drag unimplemented",t),[];case"Line":return H(t,["start","end"],o,r);default:return H(t,["center"],o,r)}},"dragShape"),H=c((t,o,[r,e],n)=>{const i=[];for(const s of o){const a=t[s];if(a.tag==="VectorV"){const[l,d]=a.contents;typeof l!="number"&&l.tag==="Input"&&(n[l.key]+=r,i.push(l.key)),typeof d!="number"&&d.tag==="Input"&&(n[d.key]+=e,i.push(d.key))}}return i},"moveProperties"),Ot=c((t,{canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),e=[];return e.push(...E(t,r)),e.push(...rt(t,o,r)),e.push(...R(t,r)),e.push(...M(t,r)),r.setAttribute("rx",t.rx.contents.toString()),e.push("rx"),r.setAttribute("ry",t.ry.contents.toString()),e.push("ry"),x(t,r,e),r},"RenderEllipse"),Tt=c((t,{canvasSize:o,labels:r})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),n=[];n.push(...V(t,o,e)),n.push(...st(t,o,e)),n.push(...M(t,e));let i=!1;const s=r.get(_(t.name));if(s&&s.tag==="EquationData"){const a=s.rendered.cloneNode(!0),l=a.getElementsByTagName("g")[0];n.push(...E(t,l)),n.push(...O(t,a)),l.setAttribute("stroke","none"),l.setAttribute("stroke-width","0");const d=t.fontSize;a.setAttribute("style",`font-size: ${d.contents}`),e.appendChild(a),i=!0}if(!i){const a=document.createElementNS("http://www.w3.org/2000/svg","text");a.textContent=_(t.string),n.push("string"),e.appendChild(a),n.push(...E(t,e)),n.push(...O(t,e))}return x(t,e,n),e},"RenderEquation"),$t=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let Dt=1;const Vt=c((t,o)=>{const r="--inject-",e={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},n=r+Dt++,i=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,s=t.querySelectorAll("[id]");let a;const l=o?[]:null;let d;const h={},p=[];let y=!1,u,f;if(s.length){for(u=0;u<s.length;u++)d=s[u].localName,d in e&&(h[d]=1);for(d in h)(e[d]||[d]).forEach(function(A){p.indexOf(A)<0&&p.push(A)});p.length&&p.push("style");const S=t.getElementsByTagName("*");let g=t,v,w,m;for(u=-1;g!==null;){if(g.localName==="style")w=g.textContent,m=w&&w.replace(i,function(A,k){return l&&(l[k]=1),"url(#"+k+n+")"}),m!==w&&(g.textContent=m);else if(g.hasAttributes()){for(f=0;f<p.length;f++)v=p[f],w=g.getAttribute(v),m=w&&w.replace(i,function(A,k){return l&&(l[k]=1),"url(#"+k+n+")"}),m&&m!==w&&g.setAttribute(v,m);for(const A of["xlink:href","href"]){let k=g.getAttribute(A);k&&/^\s*#/.test(k)&&(k=k.trim(),g.setAttribute(A,k+n),l&&(l[k.substring(1)]=1))}}g=S.item(++u)}for(u=0;u<s.length;u++)a=s[u],(!l||l[a.id])&&(a.id+=n,y=!0)}return y},"makeIdsUnique"),Gt=c(async(t,{canvasSize:o,pathResolver:r})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),n=[],i=t.href.contents;let s=await r(i);s===void 0&&(console.error(`Could not resolve image path ${i}`),s=$t),n.push("href"),e.innerHTML=s;const a=e.querySelector("svg");return Vt(e,!1),n.push(...O(t,a)),n.push(...V(t,o,e)),n.push(...st(t,o,e)),n.push(...M(t,e)),x(t,e,n),e},"RenderImage"),$=c((t,o,r,e,n,i)=>{const s=document.createElementNS("http://www.w3.org/2000/svg","marker");s.setAttribute("id",t),s.setAttribute("markerUnits","strokeWidth"),s.setAttribute("markerWidth",Y(e.width*n).toString()),s.setAttribute("markerHeight",Y(e.height*n).toString()),s.setAttribute("viewBox",e.viewbox),s.setAttribute("refX",e.refX.toString()),s.setAttribute("refY",e.refY.toString()),i?s.setAttribute("orient","auto"):s.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",e.path),e.fillKind==="stroke"?(a.setAttribute("fill","none"),s.setAttribute("stroke",o),s.setAttribute("stroke-opacity",r.toString())):(a.setAttribute("fill",o),a.setAttribute("fill-opacity",r.toString())),e.style&&Object.entries(e.style).forEach(([l,d])=>{a.setAttribute(l,d)}),s.appendChild(a),s},"arrowHead"),qt=c((t,o,r)=>{const e=[],[n,i]=[t.start.contents[0],t.start.contents[1]],[s,a]=[t.end.contents[0],t.end.contents[1]],l=t.startArrowheadSize.contents,d=t.endArrowheadSize.contents,h=t.strokeWidth.contents;e.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const p=Math.sqrt((n-s)**2+(i-a)**2);let y,u;if(o){const v=(t.flipStartArrowhead.contents?o.refX:o.width-o.refX)*l*h,w=v/p*(n-s),m=v/p*(i-a);[y,u]=[n-w,i-m]}else[y,u]=[n,i];let f,S;if(r){const g=(r.width-r.refX)*d*h;[f,S]=[s-g/p*(s-n),a-g/p*(a-i)]}else[f,S]=[s,a];return[[[y,u],[f,S]],e]},"makeRoomForArrows"),Ft=c((t,{canvasSize:o,namespace:r,variation:e})=>{const n=T(t.startArrowhead.contents),i=T(t.endArrowhead.contents),[[[s,a],[l,d]],h]=qt(t,n,i),[p,y]=C([s,a],o),[u,f]=C([l,d],o),S=`M ${p} ${y} L ${u} ${f}`,g=I(t.strokeColor.contents),v=t.strokeWidth.contents,w=j(t.strokeColor.contents),m=document.createElementNS("http://www.w3.org/2000/svg","g"),A=`${r}-${e}-${t.name.contents}`,k=A+"-startArrowId",N=A+"-endArrowId";if(n){const z=t.startArrowheadSize.contents,G=t.flipStartArrowhead.contents;m.appendChild($(k,g,w,n,z,G))}if(i){const z=t.endArrowheadSize.contents;m.appendChild($(N,g,w,i,z,!1))}h.push("strokeColor","strokeWidth","startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize");const b=document.createElementNS("http://www.w3.org/2000/svg","path");return b.setAttribute("d",S),t.strokeColor.contents.tag!=="NONE"&&(b.setAttribute("stroke-opacity",w.toString()),b.setAttribute("stroke-width",v.toString())),b.setAttribute("stroke",g),t.strokeDasharray.contents!==""?b.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&b.setAttribute("stroke-dasharray",W.toString()),h.push("strokeDasharray","strokeStyle"),t.strokeLinecap.contents!==""?b.setAttribute("stroke-linecap",t.strokeLinecap.contents):b.setAttribute("stroke-linecap","butt"),h.push("strokeLinecap"),n&&(b.setAttribute("marker-start",`url(#${k})`),h.push("startArrowhead")),i&&(b.setAttribute("marker-end",`url(#${N})`),h.push("endArrowhead")),m.appendChild(b),h.push(...M(t,m)),x(t,m,h),m},"RenderLine"),Wt=c((t,o)=>t.map(r=>{const{cmd:e,contents:n}=r;if(n.length===0&&e!=="Z")return console.error("WARNING: empty path"),"";const i=gt.flatten(n.map(s=>{switch(s.tag){case"CoordV":return C([s.contents[0],s.contents[1]],o);case"ValueV":return s.contents}})).join(" ");return`${e} ${i}`}).join(" "),"toPathString"),Xt=c(t=>{const o=document.createElementNS("http://www.w3.org/2000/svg","filter");return o.setAttribute("id",t),o.setAttribute("x","0"),o.setAttribute("y","0"),o.setAttribute("width","200%"),o.setAttribute("height","200%"),o.innerHTML=`
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
    `,o},"Shadow"),_t=c((t,{canvasSize:o})=>{const r=t.name.contents+"-startArrowId",e=t.name.contents+"-endArrowId",n=t.name.contents+"-shadow",i=document.createElementNS("http://www.w3.org/2000/svg","g"),s=t.strokeWidth.contents,a=I(t.strokeColor.contents),l=j(t.strokeColor.contents),d=I(t.fillColor.contents),h=j(t.fillColor.contents),p=[],y=T(t.startArrowhead.contents),u=T(t.endArrowhead.contents);if(y){const S=t.name.contents+"-startArrowId",g=t.startArrowheadSize.contents,v=t.flipStartArrowhead.contents;i.appendChild($(S,a,l,y,g,v))}if(u){const S=t.name.contents+"-endArrowId",g=t.endArrowheadSize.contents;i.appendChild($(S,a,l,u,g,!1))}p.push("name","strokeColor","startArrowhead","flipStartArrowhead","endArrowhead"),i.appendChild(Xt(n));const f=document.createElementNS("http://www.w3.org/2000/svg","path");return f.setAttribute("stroke",a),f.setAttribute("fill",d),p.push("fillColor","strokeColor"),t.strokeColor.contents.tag!=="NONE"&&(f.setAttribute("stroke-width",s.toString()),f.setAttribute("stroke-opacity",l.toString()),p.push("strokeColor","strokeWidth")),t.fillColor.contents.tag!=="NONE"&&(f.setAttribute("fill-opacity",h.toString()),p.push("fillColor")),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?f.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&f.setAttribute("stroke-dasharray",W.toString()),p.push("strokeDasharray","strokeStyle"),f.setAttribute("d",Wt(t.d.contents,o)),p.push("d"),y&&(f.setAttribute("marker-start",`url(#${r})`),p.push("startArrowhead")),u&&(f.setAttribute("marker-end",`url(#${e})`),p.push("endArrowhead")),i.appendChild(f),p.push(...M(t,i)),x(t,i,p),i},"RenderPath"),Yt=c((t,{canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","polygon"),e=[];return e.push(...E(t,r)),e.push(...R(t,r)),e.push(...M(t,r)),e.push(...nt(t,r)),e.push(...it(t,o,r)),x(t,r,e),r},"RenderPolygon"),Bt=c((t,{canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","polyline"),e=[];return e.push(...E(t,r)),e.push(...R(t,r)),e.push(...M(t,r)),e.push(...nt(t,r)),e.push(...it(t,o,r)),x(t,r,e),r},"RenderPolyline"),Ut=c((t,{canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","rect"),e=[];return e.push(...Et(t,o,r)),e.push(...O(t,r)),e.push(...E(t,r)),e.push(...R(t,r)),e.push(...M(t,r)),e.push(...Lt(t,r)),e.push(...V(t,o,r)),x(t,r,e),r},"RenderRectangle"),Pt=c((t,{canvasSize:o,labels:r})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text"),n=[];n.push("x","y"),n.push(...E(t,e)),n.push(...R(t,e)),n.push(...M(t,e)),n.push(...Rt(t,e)),n.push(...V(t,o,e)),n.push(...Nt(t,e));const i=t.name,s=r.get(i.contents),a=t.center,[l,d]=C([a.contents[0],a.contents[1]],o);if(s&&s.tag==="TextData"){const h=s.descent.contents,p=s.height.contents,y=d+(p/2-h);e.setAttribute("x",l.toString()),e.setAttribute("y",y.toString()),n.push(...O(t,e))}else e.setAttribute("x",l.toString()),e.setAttribute("y",d.toString());return e.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),e.setAttribute("alignment-baseline",t.alignmentBaseline.contents),e.setAttribute("dominant-baseline",t.dominantBaseline.contents),e.setAttribute("ascent",t.ascent.contents.toString()),e.setAttribute("descent",t.descent.contents.toString()),e.setAttribute("text-anchor",t.textAnchor.contents.toString()),e.setAttribute("visibility",t.visibility.contents),n.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),x(t,e,n),e},"RenderText"),K=c(({clientX:t,clientY:o},r)=>{const e=r.getScreenCTM();return e!==null?{x:(t-e.e)/e.a,y:(o-e.f)/e.d}:{x:0,y:0}},"getPosition"),Ht=c(async(t,o,r,e)=>{const n=document.createElementNS("http://www.w3.org/2000/svg","svg");n.setAttribute("xmlns","http://www.w3.org/2000/svg"),n.setAttribute("width","100%"),n.setAttribute("height","100%"),n.setAttribute("version","1.2"),n.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const i=c((a,l,d)=>{o(jt(t,a,l,d))},"onDrag"),s=t.computeShapes(t.varyingValues);return await ct(s,n,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:e,pathResolver:r},{updateState:o,onDrag:i,parentSVG:n}),n},"RenderInteractive"),Kt=c(async(t,o,r)=>{const{varyingValues:e,computeShapes:n,labelCache:i,canvas:s,variation:a}=t,l=document.createElementNS("http://www.w3.org/2000/svg","svg");l.setAttribute("version","1.2"),l.setAttribute("xmlns","http://www.w3.org/2000/svg"),l.setAttribute("viewBox",`0 0 ${s.width} ${s.height}`);const d=n(e);return await ct(d,l,{labels:i,canvasSize:s.size,variation:a,namespace:r,pathResolver:o},void 0),l},"RenderStatic"),Zt=c(async(t,o,r)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),n=kt(t.shapes);for(const i of n){const s=await at(i,o,r);e.appendChild(s)}return x(t,e,[...M(t,e),"shapes"]),e},"RenderGroup"),Jt=c(async(t,o)=>{switch(t.shapeType){case"Circle":return zt(t,o);case"Ellipse":return Ot(t,o);case"Equation":return Tt(t,o);case"Image":return Gt(t,o);case"Line":return Ft(t,o);case"Path":return _t(t,o);case"Polygon":return Yt(t,o);case"Polyline":return Bt(t,o);case"Rectangle":return Ut(t,o);case"Text":return Pt(t,o)}},"RenderShapeSvg"),at=c(async(t,o,r)=>{if(t.shapeType==="Group")return await Zt(t,o,r);{const e=await Jt(t,o);if(r){const n=document.createElementNS("http://www.w3.org/2000/svg","g");mt(t)?n.setAttribute("pointer-events","visibleStroke"):yt(t)?n.setAttribute("pointer-events","bounding-box"):n.setAttribute("pointer-events","auto"),n.appendChild(e);const i=c(s=>{const{clientX:a,clientY:l}=s,{x:d,y:h}=K({clientX:a,clientY:l},r.parentSVG),{width:p,height:y,x:u,y:f}=s.target.getBBox({stroke:!0}),S=d-u,g=o.canvasSize[0]-p+(d-u),v=h-f,w=o.canvasSize[1]-y+(h-f);n.setAttribute("opacity","0.5");let m=0,A=0;const k=c(b=>{const{x:z,y:G}=K(b,r.parentSVG),pt=Z(z,S,g),ut=Z(G,v,w);m=pt-d,A=h-ut,n.setAttribute("transform",`translate(${m},${-A})`)},"onMouseMove"),N=c(()=>{n.setAttribute("opacity","1"),document.removeEventListener("mouseup",N),document.removeEventListener("mousemove",k),r.onDrag(t.name.contents,m,A)},"onMouseUp");document.addEventListener("mouseup",N),document.addEventListener("mousemove",k)},"onMouseDown");return n.addEventListener("mousedown",i),n}else return e}},"RenderShape"),ct=c(async(t,o,r,e)=>{for(const n of t){const i=await at(n,r,e);o.appendChild(i)}},"RenderShapes"),Z=c((t,o,r)=>Math.min(Math.max(t,o),r),"clamp"),Qt=c(t=>{const o=wt(t.variation),{constraintSets:r,optStages:e}=t,{inputMask:n,objMask:i,constrMask:s}=Q(r.get(e[0]),"missing first stage");return tt(Object.assign(Object.assign({},t),{varyingValues:t.inputs.map(a=>a.init.tag==="Sampled"?a.init.sampler(o):a.init.pending),currentStageIndex:0,params:F(n,i,s)}))},"resample"),lt=c((t,o=1e4)=>{const r=Object.assign(Object.assign({},t),t.gradient.step(t,o));return D(r)&&!X(r)?dt(r):r},"stepState"),dt=c(t=>{if(X(t))return t;{const{constraintSets:o,optStages:r,currentStageIndex:e}=t,n=r[e+1],{inputMask:i,objMask:s,constrMask:a}=Q(o.get(n),"missing next stage");return Object.assign(Object.assign({},t),{currentStageIndex:e+1,params:F(i,s,a)})}},"nextStage"),te=c((t,o=1e4)=>{let r=t;for(;r.params.optStatus!=="Error"&&(!D(r)||!X(r));)D(r)&&(r=dt(r)),r=lt(r,o);return r.params.optStatus==="Error"?et(Object.assign({errorType:"RuntimeError"},At("",r))):St(r)},"stepUntilConvergence"),ee=c(async t=>{const o=bt(t.domain),r=vt(n=>xt(t.substance,n),o);return r.isErr()?et(r.error):await Mt(t.variation,t.style,...r.value)},"compileTrio"),oe=c(async t=>{const o=await Ct(t.shapes);if(o.isErr())throw Error(ot(o.error));return tt(Object.assign(Object.assign({},t),{labelCache:o.value}))},"prepareState"),D=c(t=>t.params.optStatus==="EPConverged","stateConverged"),X=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function J(t){const o=await fetch(t);if(!o.ok){console.error(`could not fetch ${t}`);return}return await o.text()}c(J,"fetchResolver");class q extends B.Component{canvasRef=B.createRef();penroseState=void 0;timerID=void 0;constructor(o){super(o),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const o=await ee(this.props);o.isOk()?(this.penroseState=await oe(o.value),this.setState({error:void 0})):this.setState({error:o.error})};converge=async()=>{if(this.penroseState){const o=te(this.penroseState);o.isOk()?this.penroseState=o.value:this.setState({error:o.error})}};tick=()=>{this.props.animate&&this.penroseState&&!D(this.penroseState)&&(this.penroseState=lt(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async o=>{if(this.props.domain!==o.domain||this.props.substance!==o.substance||this.props.style!==o.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==o.variation||this.props.animate!==o.animate){this.penroseState.variation=this.props.variation,this.penroseState=Qt(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==o.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return L("div",{children:"rendering..."});{const o=this.canvasRef.current;if(this.penroseState){const r=await(this.props.interactive===!1?Kt(this.penroseState,this.props.imageResolver??J,this.props.name??""):Ht(this.penroseState,async e=>{this.penroseState=e,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??J,this.props.name??""));r.setAttribute("height","100%"),r.setAttribute("width","100%"),o.firstChild!==null?o.replaceChild(r,o.firstChild):o.appendChild(r),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return L("div",{children:"rendering..."})}};render=()=>{const{error:o}=this.state;return U("div",{style:{width:"100%",height:"100%"},children:[!o&&L("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),o&&U("div",{style:{padding:"1em",height:"100%"},children:[L("div",{style:{fontWeight:700},children:"1 error:"}),L("div",{style:{fontFamily:"monospace"},children:ot(o).toString().split(`
`).map((r,e)=>L("p",{style:{margin:0},children:r},`err-ln-${e}`))})]})]})}}c(q,"Simple");try{q.displayName="Simple",q.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{q as S};
//# sourceMappingURL=Simple-d11c9982.js.map
