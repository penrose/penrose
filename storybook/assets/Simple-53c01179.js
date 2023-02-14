var vt=Object.defineProperty;var c=(t,e)=>vt(t,"name",{value:e,configurable:!0});import{g as F,t as j,b as z,c as C,d as St,e as _,f as $,r as Y,l as bt,s as B,h as xt,i as nt,j as it,k as st,n as Mt,o as Ct,m as Et,p as Nt,q as Lt,u as It,w as Rt,x as at}from"./PenrosePrograms-69c6181f.js";import{R as H}from"./index-74c5fbfa.js";import{j as R,a as Z}from"./jsx-runtime-9c5bc5e6.js";const jt=c((t,e,o,r)=>{const n=[...t.varyingValues],{constraintSets:s,optStages:i}=t,{inputMask:a,objMask:l,constrMask:d}=s.get(i[0]),w=[...a];for(const m of t.shapes)if(m.properties.name.contents===e)for(const u of zt(m,[o,r],n))w[u]=!1;return Object.assign(Object.assign({},t),{params:F(w,l,d),varyingValues:n})},"dragUpdate"),zt=c((t,e,o)=>{const{shapeType:r,properties:n}=t;switch(r){case"Path":return console.log("Path drag unimplemented",t),[];case"Polygon":return console.log("Polygon drag unimplemented",t),[];case"Polyline":return console.log("Polyline drag unimplemented",t),[];case"Line":return K(n,["start","end"],e,o);default:return K(n,["center"],e,o)}},"dragShape"),K=c((t,e,[o,r],n)=>{const s=[];for(const i of e){const a=t[i];if(a.tag==="VectorV"){const[l,d]=a.contents;typeof l!="number"&&l.tag==="Input"&&(n[l.key]+=o,s.push(l.key)),typeof d!="number"&&d.tag==="Input"&&(n[d.key]+=r,s.push(d.key))}}return s},"moveProperties"),J={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},M=c(({properties:t},e,o)=>{const r=["strokeStyle","name","ensureOnCanvas"],n=new Set(o.concat(r));for(const s in t){const i=t[s].contents.toString();if(i!==""&&!n.has(s))if(s in J){const a=J[s];e.hasAttribute(a)||e.setAttribute(a,i)}else if(s==="style"&&i!==""){const a=e.getAttribute(s);a===null?e.setAttribute(s,i):e.setAttribute(s,`${a}${i}`)}else e.hasAttribute(s)||e.setAttribute(s,i)}},"attrAutoFillSvg"),N=c(({properties:t},e)=>{const o=t.fillColor,r=j(o.contents);return e.setAttribute("fill",z(o.contents)),o.contents.tag!=="NONE"&&e.setAttribute("fill-opacity",r.toString()),["fillColor"]},"attrFill"),ct=c(({properties:t},e,o)=>{const r=t.center,[n,s]=C(r.contents,e);return o.setAttribute("cx",n.toString()),o.setAttribute("cy",s.toString()),["center"]},"attrCenter"),lt=c(({properties:t},e)=>{let o=t.scale.contents;o=o||1;let r=e.getAttribute("transform");return r=r===null?`scale(${o})`:r+`scale{${o}}`,e.setAttribute("transform",r),["scale"]},"attrScale"),dt=c(({properties:t},e,o)=>{const r=t.center,[n,s]=C(r.contents,e),i=t.width,a=t.height;let l=o.getAttribute("transform");return l=l===null?`translate(${n-i.contents/2}, ${s-a.contents/2})`:l+`translate(${n-i.contents/2}, ${s-a.contents/2})`,o.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),Ot=c(({properties:t},e,o)=>{const r=t.center,[n,s]=C(r.contents,e),i=t.width,a=t.height;return o.setAttribute("x",(n-i.contents/2).toString()),o.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),G=c(({properties:t},e,o)=>{const r=t.width,n=t.height,s=t.center,i=t.rotation.contents,[a,l]=C(s.contents,e);let d=o.getAttribute("transform");return d=d===null?`rotate(${i}, ${a-r.contents/2}, ${l-n.contents/2})`:d+`rotate(${i}, ${a-r.contents/2}, ${l-n.contents/2})`,o.setAttribute("transform",d),["rotation","center","width","height"]},"attrRotation"),O=c(({properties:t},e)=>{const o=t.width,r=t.height;return e.setAttribute("width",o.contents.toString()),e.setAttribute("height",r.contents.toString()),["width","height"]},"attrWH"),Tt=c(({properties:t},e)=>{const o=t.cornerRadius;return e.setAttribute("rx",o.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Dt=c(({properties:t},e)=>{const o=t.string,r=document.createTextNode(o.contents.toString());return e.appendChild(r),["string"]},"attrString"),W="7,5",I=c(({properties:t},e)=>{const o=[],r=t.strokeColor,n=j(r.contents),s=t.strokeWidth.contents;return e.setAttribute("stroke",z(r.contents)),o.push("strokeColor","strokeWidth"),r.contents.tag!=="NONE"&&(e.setAttribute("stroke-opacity",n.toString()),e.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?e.setAttribute("stroke-dasharray",t.strokeDasharray.contents):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(e.setAttribute("stroke-dasharray",W.toString()),o.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""?e.setAttribute("stroke-linecap",t.strokeLinecap.contents):e.setAttribute("stroke-linecap","butt"),o.push("strokeLinecap")),o},"attrStroke"),E=c(({properties:t},e)=>{const o=t.name,r=document.createElementNS("http://www.w3.org/2000/svg","title");return r.textContent=o.contents,e.appendChild(r),["name"]},"attrTitle"),pt=c((t,e)=>{const o=St(t),r=e.getAttribute("style");return e.setAttribute("style",r?`${r}; font: ${o};`:`font: ${o};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),ut=c((t,e,o)=>{const n=t.properties.points.contents.map(s=>C(s,e));return o.setAttribute("points",n.toString()),["points"]},"attrPolyPoints"),$t=c(({shape:t,canvasSize:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","circle"),r=[];return r.push(...N(t,o)),r.push(...ct(t,e,o)),r.push(...I(t,o)),r.push(...E(t,o)),M(t,o,r),o},"Circle"),Vt=c(({shape:t,canvasSize:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),r=[];return r.push(...N(t,o)),r.push(...ct(t,e,o)),r.push(...I(t,o)),r.push(...E(t,o)),M(t,o,r),o},"Ellipse"),Ut=c(({shape:t,canvasSize:e,labels:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","g"),n=[];n.push(...G(t,e,r)),n.push(...dt(t,e,r)),n.push(...E(t,r));let s=!1;const i=o.get(_(t.properties.name));if(i&&i.tag==="EquationData"){const a=i.rendered.cloneNode(!0),l=a.getElementsByTagName("g")[0];n.push(...N(t,l)),n.push(...O(t,a)),l.setAttribute("stroke","none"),l.setAttribute("stroke-width","0");const d=t.properties.fontSize;a.setAttribute("style",`font-size: ${d.contents}`),r.appendChild(a),s=!0}if(!s){const a=document.createElementNS("http://www.w3.org/2000/svg","text");a.textContent=_(t.properties.string),n.push("string"),r.appendChild(a),n.push(...N(t,r)),n.push(...O(t,r)),n.push(...I(t,r)),n.push(...pt(t,r))}return M(t,r,n),r},"Equation"),Gt=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let Pt=1;const Ft=c((t,e)=>{const o="--inject-",r={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},n=o+Pt++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const l=e?[]:null;let d;const w={},p=[];let m=!1,u,h;if(i.length){for(u=0;u<i.length;u++)d=i[u].localName,d in r&&(w[d]=1);for(d in w)(r[d]||[d]).forEach(function(x){p.indexOf(x)<0&&p.push(x)});p.length&&p.push("style");const A=t.getElementsByTagName("*");let f=t,v,g,b;for(u=-1;f!==null;){if(f.localName==="style")g=f.textContent,b=g&&g.replace(s,function(x,y){return l&&(l[y]=1),"url(#"+y+n+")"}),b!==g&&(f.textContent=b);else if(f.hasAttributes()){for(h=0;h<p.length;h++)v=p[h],g=f.getAttribute(v),b=g&&g.replace(s,function(x,y){return l&&(l[y]=1),"url(#"+y+n+")"}),b&&b!==g&&f.setAttribute(v,b);for(const x of["xlink:href","href"]){let y=f.getAttribute(x);y&&/^\s*#/.test(y)&&(y=y.trim(),f.setAttribute(x,y+n),l&&(l[y.substring(1)]=1))}}f=A.item(++u)}for(u=0;u<i.length;u++)a=i[u],(!l||l[a.id])&&(a.id+=n,m=!0)}return m},"makeIdsUnique"),Wt=c(async({shape:t,canvasSize:e,pathResolver:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","g"),n=[],s=t.properties.href.contents;let i=await o(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=Gt),n.push("href"),r.innerHTML=i;const a=r.querySelector("svg");return Ft(r,!1),n.push(...O(t,a)),n.push(...G(t,e,r)),n.push(...dt(t,e,r)),M(t,r,n),r},"Image");let D;const qt=new Uint8Array(16);function Xt(){if(!D&&(D=typeof crypto<"u"&&crypto.getRandomValues&&crypto.getRandomValues.bind(crypto),!D))throw new Error("crypto.getRandomValues() not supported. See https://github.com/uuidjs/uuid#getrandomvalues-not-supported");return D(qt)}c(Xt,"rng");const k=[];for(let t=0;t<256;++t)k.push((t+256).toString(16).slice(1));function _t(t,e=0){return(k[t[e+0]]+k[t[e+1]]+k[t[e+2]]+k[t[e+3]]+"-"+k[t[e+4]]+k[t[e+5]]+"-"+k[t[e+6]]+k[t[e+7]]+"-"+k[t[e+8]]+k[t[e+9]]+"-"+k[t[e+10]]+k[t[e+11]]+k[t[e+12]]+k[t[e+13]]+k[t[e+14]]+k[t[e+15]]).toLowerCase()}c(_t,"unsafeStringify");const Yt=typeof crypto<"u"&&crypto.randomUUID&&crypto.randomUUID.bind(crypto),Q={randomUUID:Yt};function Bt(t,e,o){if(Q.randomUUID&&!e&&!t)return Q.randomUUID();t=t||{};const r=t.random||(t.rng||Xt)();if(r[6]=r[6]&15|64,r[8]=r[8]&63|128,e){o=o||0;for(let n=0;n<16;++n)e[o+n]=r[n];return e}return _t(r)}c(Bt,"v4");const V=c((t,e,o,r,n,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",Y(r.width*n).toString()),i.setAttribute("markerHeight",Y(r.height*n).toString()),i.setAttribute("viewBox",r.viewbox),i.setAttribute("refX",r.refX.toString()),i.setAttribute("refY",r.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",r.path),r.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",e),i.setAttribute("stroke-opacity",o.toString())):(a.setAttribute("fill",e),a.setAttribute("fill-opacity",o.toString())),r.style&&Object.entries(r.style).forEach(([l,d])=>{a.setAttribute(l,d)}),i.appendChild(a),i},"arrowHead"),Ht=c((t,e,o)=>{const r=[],[n,s]=t.properties.start.contents,[i,a]=t.properties.end.contents,l=t.properties.startArrowheadSize.contents,d=t.properties.endArrowheadSize.contents,w=t.properties.strokeWidth.contents;r.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const p=Math.sqrt((n-i)**2+(s-a)**2);let m,u;if(e){const v=(t.properties.flipStartArrowhead.contents?e.refX:e.width-e.refX)*l*w,g=v/p*(n-i),b=v/p*(s-a);[m,u]=[n-g,s-b]}else[m,u]=[n,s];let h,A;if(o){const f=(o.width-o.refX)*d*w;[h,A]=[i-f/p*(i-n),a-f/p*(a-s)]}else[h,A]=[i,a];return[[[m,u],[h,A]],r]},"makeRoomForArrows"),Zt=c(({shape:t,canvasSize:e,variation:o})=>{const r=$(t.properties.startArrowhead.contents),n=$(t.properties.endArrowhead.contents),[[[s,i],[a,l]],d]=Ht(t,r,n),[w,p]=C([s,i],e),[m,u]=C([a,l],e),h=`M ${w} ${p} L ${m} ${u}`,A=z(t.properties.strokeColor.contents),f=t.properties.strokeWidth.contents,v=j(t.properties.strokeColor.contents),g=document.createElementNS("http://www.w3.org/2000/svg","g"),b=Bt(),x=b+"-startArrowId",y=b+"-endArrowId";if(r){const L=t.properties.startArrowheadSize.contents,T=t.properties.flipStartArrowhead.contents;g.appendChild(V(x,A,v,r,L,T))}if(n){const L=t.properties.endArrowheadSize.contents;g.appendChild(V(y,A,v,n,L,!1))}d.push("strokeColor","strokeWidth","startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize");const S=document.createElementNS("http://www.w3.org/2000/svg","path");return S.setAttribute("d",h),t.properties.strokeColor.contents.tag!=="NONE"&&(S.setAttribute("stroke-opacity",v.toString()),S.setAttribute("stroke-width",f.toString())),S.setAttribute("stroke",A),"strokeDasharray"in t.properties&&t.properties.strokeDasharray.contents!==""?S.setAttribute("stroke-dasharray",t.properties.strokeDasharray.contents):t.properties.strokeStyle.contents==="dashed"&&S.setAttribute("stroke-dasharray",W.toString()),d.push("strokeDasharray","strokeStyle"),"strokeLinecap"in t.properties&&t.properties.strokeLinecap.contents!==""?S.setAttribute("stroke-linecap",t.properties.strokeLinecap.contents):S.setAttribute("stroke-linecap","butt"),d.push("strokeLinecap"),r&&(S.setAttribute("marker-start",`url(#${x})`),d.push("startArrowhead")),n&&(S.setAttribute("marker-end",`url(#${y})`),d.push("endArrowhead")),g.appendChild(S),d.push(...E(t,g)),M(t,g,d),g},"Line"),Kt=c((t,e)=>t.map(o=>{const{cmd:r,contents:n}=o;if(n.length===0&&r!=="Z")return console.error("WARNING: empty path"),"";const s=bt.flatten(n.map(i=>{switch(i.tag){case"CoordV":return C(i.contents,e);case"ValueV":return i.contents}})).join(" ");return`${r} ${s}`}).join(" "),"toPathString"),Jt=c(t=>{const e=document.createElementNS("http://www.w3.org/2000/svg","filter");return e.setAttribute("id",t),e.setAttribute("x","0"),e.setAttribute("y","0"),e.setAttribute("width","200%"),e.setAttribute("height","200%"),e.innerHTML=`
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
    `,e},"Shadow"),Qt=c(({shape:t,canvasSize:e})=>{const o=t.properties.name.contents+"-startArrowId",r=t.properties.name.contents+"-endArrowId",n=t.properties.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=t.properties.strokeWidth.contents,a=z(t.properties.strokeColor.contents),l=j(t.properties.strokeColor.contents),d=z(t.properties.fillColor.contents),w=j(t.properties.fillColor.contents),p=[],m=$(t.properties.startArrowhead.contents),u=$(t.properties.endArrowhead.contents);if(m){const A=t.properties.name.contents+"-startArrowId",f=t.properties.startArrowheadSize.contents,v=t.properties.flipStartArrowhead.contents;s.appendChild(V(A,a,l,m,f,v))}if(u){const A=t.properties.name.contents+"-endArrowId",f=t.properties.endArrowheadSize.contents;s.appendChild(V(A,a,l,u,f,!1))}p.push("name","strokeColor","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(Jt(n));const h=document.createElementNS("http://www.w3.org/2000/svg","path");return h.setAttribute("stroke",a),h.setAttribute("fill",d),p.push("fillColor","strokeColor"),t.properties.strokeColor.contents.tag!=="NONE"&&(h.setAttribute("stroke-width",i.toString()),h.setAttribute("stroke-opacity",l.toString()),p.push("strokeColor","strokeWidth")),t.properties.fillColor.contents.tag!=="NONE"&&(h.setAttribute("fill-opacity",w.toString()),p.push("fillColor")),"strokeDasharray"in t.properties&&t.properties.strokeDasharray.contents!==""?h.setAttribute("stroke-dasharray",t.properties.strokeDasharray.contents):t.properties.strokeStyle.contents==="dashed"&&h.setAttribute("stroke-dasharray",W.toString()),p.push("strokeDasharray","strokeStyle"),h.setAttribute("d",Kt(t.properties.d.contents,e)),p.push("d"),m&&(h.setAttribute("marker-start",`url(#${o})`),p.push("startArrowhead")),u&&(h.setAttribute("marker-end",`url(#${r})`),p.push("endArrowhead")),s.appendChild(h),p.push(...E(t,s)),M(t,s,p),s},"Path"),te=c(({shape:t,canvasSize:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","polygon"),r=[];return r.push(...N(t,o)),r.push(...I(t,o)),r.push(...E(t,o)),r.push(...lt(t,o)),r.push(...ut(t,e,o)),M(t,o,r),o},"Polygon"),ee=c(({shape:t,canvasSize:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","polyline"),r=[];return r.push(...N(t,o)),r.push(...I(t,o)),r.push(...E(t,o)),r.push(...lt(t,o)),r.push(...ut(t,e,o)),M(t,o,r),o},"Polyline"),re=c(({shape:t,canvasSize:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","rect"),r=[];return r.push(...Ot(t,e,o)),r.push(...O(t,o)),r.push(...N(t,o)),r.push(...I(t,o)),r.push(...E(t,o)),r.push(...Tt(t,o)),r.push(...G(t,e,o)),M(t,o,r),o},"Rectangle"),oe=c(({shape:t,canvasSize:e,labels:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","text"),n=[];n.push("x","y"),n.push(...N(t,r)),n.push(...I(t,r)),n.push(...E(t,r)),n.push(...Dt(t,r)),n.push(...G(t,e,r)),n.push(...pt(t,r));const s=t.properties.name,i=o.get(s.contents),a=t.properties.center,[l,d]=C(a.contents,e);if(i&&i.tag==="TextData"){const w=i.descent.contents,p=i.height.contents,m=d+(p/2-w);r.setAttribute("x",l.toString()),r.setAttribute("y",m.toString()),n.push(...O(t,r))}else r.setAttribute("x",l.toString()),r.setAttribute("y",d.toString());return M(t,r,n),r},"Text"),tt={Circle:$t,Ellipse:Vt,Rectangle:re,Polygon:te,Polyline:ee,Equation:Ut,Path:Qt,Line:Zt,Image:Wt,Text:oe},ht=c(async({shape:t,labels:e,canvasSize:o,variation:r,pathResolver:n})=>t.shapeType in tt?await tt[t.shapeType]({shape:t,labels:e,variation:r,canvasSize:o,pathResolver:n}):(console.error(`${t.shapeType} shape doesn't exist in shapeMap`),document.createElementNS("http://www.w3.org/2000/svg","g")),"RenderShape"),et=c(({clientX:t,clientY:e},o)=>{const r=o.getScreenCTM();return r!==null?{x:(t-r.e)/r.a,y:(e-r.f)/r.d}:{x:0,y:0}},"getPosition"),ne=c(async(t,e,o,r)=>{const n=t.canvasSize,s=await ht(Object.assign(Object.assign({},t),{canvasSize:r||n})),i=document.createElementNS("http://www.w3.org/2000/svg","g"),{shapeType:a}=t.shape;B[a].isLinelike?i.setAttribute("pointer-events","visibleStroke"):B[a].isRectlike?i.setAttribute("pointer-events","bounding-box"):i.setAttribute("pointer-events","auto"),i.appendChild(s);const l=c(d=>{const{clientX:w,clientY:p}=d,{x:m,y:u}=et({clientX:w,clientY:p},o),{width:h,height:A,x:f,y:v}=d.target.getBBox({stroke:!0}),g=m-f,b=n[0]-h+(m-f),x=u-v,y=n[1]-A+(u-v);i.setAttribute("opacity","0.5");let S=0,L=0;const T=c(gt=>{const{x:yt,y:kt}=et(gt,o),wt=rt(yt,g,b),At=rt(kt,x,y);S=wt-m,L=u-At,i.setAttribute("transform",`translate(${S},${-L})`)},"onMouseMove"),X=c(()=>{i.setAttribute("opacity","1"),document.removeEventListener("mouseup",X),document.removeEventListener("mousemove",T),e(t.shape.properties.name.contents,S,L)},"onMouseUp");document.addEventListener("mouseup",X),document.addEventListener("mousemove",T)},"onMouseDown");return i.addEventListener("mousedown",l),i},"DraggableShape"),ie=c(async(t,e,o)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","svg");r.setAttribute("xmlns","http://www.w3.org/2000/svg"),r.setAttribute("width","100%"),r.setAttribute("height","100%"),r.setAttribute("version","1.2"),r.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const n=c((s,i,a)=>{e(jt(t,s,i,a))},"onDrag");for(const s of t.computeShapes(t.varyingValues))r.appendChild(await ne({shape:s,labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,pathResolver:o},n,r));return r},"RenderInteractive"),se=c(async(t,e)=>{const{varyingValues:o,computeShapes:r,labelCache:n,canvas:s}=t,i=document.createElementNS("http://www.w3.org/2000/svg","svg");return i.setAttribute("version","1.2"),i.setAttribute("xmlns","http://www.w3.org/2000/svg"),i.setAttribute("viewBox",`0 0 ${s.width} ${s.height}`),Promise.all(r(o).map(a=>ht({shape:a,labels:n,canvasSize:s.size,variation:t.variation,pathResolver:e}))).then(a=>{for(const l of a)i.appendChild(l);return i})},"RenderStatic"),rt=c((t,e,o)=>Math.min(Math.max(t,e),o),"clamp"),ae=c(t=>{const e=xt(t.variation),{constraintSets:o,optStages:r}=t,{inputMask:n,objMask:s,constrMask:i}=nt(o.get(r[0]),"missing first stage");return it(Object.assign(Object.assign({},t),{varyingValues:t.inputs.map(a=>a.init.tag==="Sampled"?a.init.sampler(e):a.init.pending),currentStageIndex:0,params:F(n,s,i)}))},"resample"),ft=c((t,e=1e4)=>{const o=Object.assign(Object.assign({},t),t.gradient.step(t,e));return U(o)&&!q(o)?mt(o):o},"stepState"),mt=c(t=>{if(q(t))return t;{const{constraintSets:e,optStages:o,currentStageIndex:r}=t,n=o[r+1],{inputMask:s,objMask:i,constrMask:a}=nt(e.get(n),"missing next stage");return Object.assign(Object.assign({},t),{currentStageIndex:r+1,params:F(s,i,a)})}},"nextStage"),ce=c((t,e=1e4)=>{let o=t;for(;o.params.optStatus!=="Error"&&(!U(o)||!q(o));)U(o)&&(o=mt(o)),o=ft(o,e);return o.params.optStatus==="Error"?st(Object.assign({errorType:"RuntimeError"},Mt("",o))):Ct(o)},"stepUntilConvergence"),le=c(async t=>{const e=Et(t.domain),o=Nt(n=>Lt(t.substance,n),e);return o.isErr()?st(o.error):await It(t.variation,t.style,...o.value)},"compileTrio"),de=c(async t=>{const e=await Rt(t.shapes);if(e.isErr())throw Error(at(e.error));return it(Object.assign(Object.assign({},t),{labelCache:e.value}))},"prepareState"),U=c(t=>t.params.optStatus==="EPConverged","stateConverged"),q=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function ot(t){const e=await fetch(t);if(!e.ok){console.error(`could not fetch ${t}`);return}return await e.text()}c(ot,"fetchResolver");class P extends H.Component{canvasRef=H.createRef();penroseState=void 0;timerID=void 0;constructor(e){super(e),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const e=await le(this.props);e.isOk()?(this.penroseState=await de(e.value),this.setState({error:void 0})):this.setState({error:e.error})};converge=async()=>{if(this.penroseState){const e=ce(this.penroseState);e.isOk()?this.penroseState=e.value:this.setState({error:e.error})}};tick=()=>{this.props.animate&&this.penroseState&&!U(this.penroseState)&&(this.penroseState=ft(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async e=>{if(this.props.domain!==e.domain||this.props.substance!==e.substance||this.props.style!==e.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==e.variation||this.props.animate!==e.animate){this.penroseState.variation=this.props.variation,this.penroseState=ae(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==e.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return R("div",{children:"rendering..."});{const e=this.canvasRef.current;if(this.penroseState){const o=await(this.props.interactive===!1?se(this.penroseState,this.props.imageResolver??ot):ie(this.penroseState,async r=>{this.penroseState=r,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??ot));e.firstChild!==null?e.replaceChild(o,e.firstChild):e.appendChild(o),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return R("div",{children:"rendering..."})}};render=()=>{const{error:e}=this.state;return Z("div",{style:{width:"100%",height:"100%"},children:[!e&&R("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),e&&Z("div",{style:{padding:"1em",height:"100%"},children:[R("div",{style:{fontWeight:700},children:"1 error:"}),R("div",{style:{fontFamily:"monospace"},children:at(e).toString().split(`
`).map((o,r)=>R("p",{style:{margin:0},children:o},`err-ln-${r}`))})]})]})}}c(P,"Simple");try{P.displayName="Simple",P.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}}}}}catch{}export{P as S};
//# sourceMappingURL=Simple-53c01179.js.map
