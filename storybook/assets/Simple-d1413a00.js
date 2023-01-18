var kt=Object.defineProperty;var c=(t,o)=>kt(t,"name",{value:o,configurable:!0});import{g as W,t as z,b as j,c as S,d as wt,e as P,f as T,r as Y,l as At,s as _,h as St,i as et,j as rt,k as ot,n as bt,o as vt,m as xt,p as Mt,q as Ct,u as Et,w as Lt,x as st}from"./PenrosePrograms-215d548c.js";import{R as B}from"./index-74c5fbfa.js";import{j as N,a as U}from"./jsx-runtime-9c5bc5e6.js";const Nt=c((t,o,r,e)=>{const s=[...t.varyingValues],{constraintSets:i,optStages:n}=t,{inputMask:a,objMask:d,constrMask:l}=i.get(n[0]),g=[...a];for(const u of t.shapes)if(u.properties.name.contents===o)for(const f of zt(u,[r,e],s))g[f]=!1;return Object.assign(Object.assign({},t),{params:W(g,d,l),varyingValues:s})},"dragUpdate"),zt=c((t,o,r)=>{const{shapeType:e,properties:s}=t;switch(e){case"Path":return console.log("Path drag unimplemented",t),[];case"Polygon":return console.log("Polygon drag unimplemented",t),[];case"Polyline":return console.log("Polyline drag unimplemented",t),[];case"Line":return H(s,["start","end"],o,r);default:return H(s,["center"],o,r)}},"dragShape"),H=c((t,o,[r,e],s)=>{const i=[];for(const n of o){const a=t[n];if(a.tag==="VectorV"){const[d,l]=a.contents;typeof d!="number"&&d.tag==="Input"&&(s[d.key]+=r,i.push(d.key)),typeof l!="number"&&l.tag==="Input"&&(s[l.key]+=e,i.push(l.key))}}return i},"moveProperties"),K={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},A=c(({properties:t},o,r)=>{const e=["strokeStyle","name","ensureOnCanvas"],s=new Set(r.concat(e));for(const i in t){const n=t[i].contents.toString();if(n!==""&&!s.has(i))if(i in K){const a=K[i];o.hasAttribute(a)||o.setAttribute(a,n)}else if(i==="style"&&n!==""){const a=o.getAttribute(i);a===null?o.setAttribute(i,n):o.setAttribute(i,`${a}${n}`)}else o.hasAttribute(i)||o.setAttribute(i,n)}},"attrAutoFillSvg"),v=c(({properties:t},o)=>{const r=t.fillColor,e=z(r.contents);return o.setAttribute("fill",j(r.contents)),r.contents.tag!=="NONE"&&o.setAttribute("fill-opacity",e.toString()),["fillColor"]},"attrFill"),nt=c(({properties:t},o,r)=>{const e=t.center,[s,i]=S(e.contents,o);return r.setAttribute("cx",s.toString()),r.setAttribute("cy",i.toString()),["center"]},"attrCenter"),it=c(({properties:t},o)=>{let r=t.scale.contents;r=r||1;let e=o.getAttribute("transform");return e=e===null?`scale(${r})`:e+`scale{${r}}`,o.setAttribute("transform",e),["scale"]},"attrScale"),at=c(({properties:t},o,r)=>{const e=t.center,[s,i]=S(e.contents,o),n=t.width,a=t.height;let d=r.getAttribute("transform");return d=d===null?`translate(${s-n.contents/2}, ${i-a.contents/2})`:d+`translate(${s-n.contents/2}, ${i-a.contents/2})`,r.setAttribute("transform",d),["center","width","height"]},"attrTransformCoords"),jt=c(({properties:t},o,r)=>{const e=t.center,[s,i]=S(e.contents,o),n=t.width,a=t.height;return r.setAttribute("x",(s-n.contents/2).toString()),r.setAttribute("y",(i-a.contents/2).toString()),["center","width","height"]},"attrXY"),I=c(({properties:t},o,r)=>{const e=t.width,s=t.height,i=t.center,n=t.rotation.contents,[a,d]=S(i.contents,o);let l=r.getAttribute("transform");return l=l===null?`rotate(${n}, ${a-e.contents/2}, ${d-s.contents/2})`:l+`rotate(${n}, ${a-e.contents/2}, ${d-s.contents/2})`,r.setAttribute("transform",l),["rotation","center","width","height"]},"attrRotation"),O=c(({properties:t},o)=>{const r=t.width,e=t.height;return o.setAttribute("width",r.contents.toString()),o.setAttribute("height",e.contents.toString()),["width","height"]},"attrWH"),Ot=c(({properties:t},o)=>{const r=t.cornerRadius;return o.setAttribute("rx",r.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Tt=c(({properties:t},o)=>{const r=t.string,e=document.createTextNode(r.contents.toString());return o.appendChild(e),["string"]},"attrString"),F="7,5",x=c(({properties:t},o)=>{const r=[],e=t.strokeColor,s=z(e.contents),i=t.strokeWidth.contents;return o.setAttribute("stroke",j(e.contents)),r.push("strokeColor","strokeWidth"),e.contents.tag!=="NONE"&&(o.setAttribute("stroke-opacity",s.toString()),o.setAttribute("stroke-width",i.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?o.setAttribute("stroke-dasharray",t.strokeDasharray.contents):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(o.setAttribute("stroke-dasharray",F.toString()),r.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""?o.setAttribute("stroke-linecap",t.strokeLinecap.contents):o.setAttribute("stroke-linecap","butt"),r.push("strokeLinecap")),r},"attrStroke"),b=c(({properties:t},o)=>{const r=t.name,e=document.createElementNS("http://www.w3.org/2000/svg","title");return e.textContent=r.contents,o.appendChild(e),["name"]},"attrTitle"),ct=c((t,o)=>{const r=wt(t),e=o.getAttribute("style");return o.setAttribute("style",e?`${e}; font: ${r};`:`font: ${r};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),dt=c((t,o,r)=>{const s=t.properties.points.contents.map(i=>S(i,o));return r.setAttribute("points",s.toString()),["points"]},"attrPolyPoints"),$t=c(({shape:t,canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","circle"),e=[];return e.push(...v(t,r)),e.push(...nt(t,o,r)),e.push(...x(t,r)),e.push(...b(t,r)),A(t,r,e),r},"Circle"),Rt=c(({shape:t,canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),e=[];return e.push(...v(t,r)),e.push(...nt(t,o,r)),e.push(...x(t,r)),e.push(...b(t,r)),A(t,r,e),r},"Ellipse"),It=c(({shape:t,canvasSize:o,labels:r})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),s=[];s.push(...I(t,o,e)),s.push(...at(t,o,e)),s.push(...b(t,e));let i=!1;const n=r.get(P(t.properties.name));if(n&&n.tag==="EquationData"){const a=n.rendered.cloneNode(!0),d=a.getElementsByTagName("g")[0];s.push(...v(t,d)),s.push(...O(t,a)),d.setAttribute("stroke","none"),d.setAttribute("stroke-width","0");const l=t.properties.fontSize;a.setAttribute("style",`font-size: ${l.contents}`),e.appendChild(a),i=!0}if(!i){const a=document.createElementNS("http://www.w3.org/2000/svg","text");a.textContent=P(t.properties.string),s.push("string"),e.appendChild(a),s.push(...v(t,e)),s.push(...O(t,e)),s.push(...x(t,e)),s.push(...ct(t,e))}return A(t,e,s),e},"Equation"),Dt=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`,Vt=c(async({shape:t,canvasSize:o,pathResolver:r})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),s=[],i=t.properties.href.contents;let n=await r(i);n===void 0&&(console.error(`Could not resolve image path ${i}`),n=Dt),s.push("href"),e.innerHTML=n;const a=e.querySelector("svg"),d=a.getElementsByTagName("defs");return d.length>0&&d[0].querySelectorAll("*").forEach(l=>{l.id!==""&&a.querySelectorAll(`[*|href="#${l.id}"]:not([href])`).forEach(p=>{const u=`${t.properties.name.contents}-ns-${l.id}`;p.setAttributeNS("http://www.w3.org/1999/xlink","href","#"+u),l.setAttribute("id",u)})}),s.push(...O(t,a)),s.push(...I(t,o,e)),s.push(...at(t,o,e)),A(t,e,s),e},"Image"),$=c((t,o,r,e,s,i)=>{const n=document.createElementNS("http://www.w3.org/2000/svg","marker");n.setAttribute("id",t),n.setAttribute("markerUnits","strokeWidth"),n.setAttribute("markerWidth",Y(e.width*s).toString()),n.setAttribute("markerHeight",Y(e.height*s).toString()),n.setAttribute("viewBox",e.viewbox),n.setAttribute("refX",e.refX.toString()),n.setAttribute("refY",e.refY.toString()),i?n.setAttribute("orient","auto"):n.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",e.path),e.fillKind==="stroke"?(a.setAttribute("fill","none"),n.setAttribute("stroke",o),n.setAttribute("stroke-opacity",r.toString())):(a.setAttribute("fill",o),a.setAttribute("fill-opacity",r.toString())),e.style&&Object.entries(e.style).forEach(([d,l])=>{a.setAttribute(d,l)}),n.appendChild(a),n},"arrowHead"),Wt=c((t,o,r)=>{const e=[],[s,i]=t.properties.start.contents,[n,a]=t.properties.end.contents,d=t.properties.startArrowheadSize.contents,l=t.properties.endArrowheadSize.contents,g=t.properties.strokeWidth.contents;e.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const p=Math.sqrt((s-n)**2+(i-a)**2);let u,f;if(o){const y=(t.properties.flipStartArrowhead.contents?o.refX:o.width-o.refX)*d*g,M=y/p*(s-n),C=y/p*(i-a);[u,f]=[s-M,i-C]}else[u,f]=[s,i];let h,w;if(r){const m=(r.width-r.refX)*l*g;[h,w]=[n-m/p*(n-s),a-m/p*(a-i)]}else[h,w]=[n,a];return[[[u,f],[h,w]],e]},"makeRoomForArrows"),Ft=c(({shape:t,canvasSize:o})=>{const r=T(t.properties.startArrowhead.contents),e=T(t.properties.endArrowhead.contents),[[[s,i],[n,a]],d]=Wt(t,r,e),[l,g]=S([s,i],o),[p,u]=S([n,a],o),f=`M ${l} ${g} L ${p} ${u}`,h=j(t.properties.strokeColor.contents),w=t.properties.strokeWidth.contents,m=z(t.properties.strokeColor.contents),y=document.createElementNS("http://www.w3.org/2000/svg","g"),M=t.properties.name.contents+"-startArrowId",C=t.properties.name.contents+"-endArrowId";if(r){const E=t.properties.startArrowheadSize.contents,L=t.properties.flipStartArrowhead.contents;y.appendChild($(M,h,m,r,E,L))}if(e){const E=t.properties.endArrowheadSize.contents;y.appendChild($(C,h,m,e,E,!1))}d.push("strokeColor","strokeWidth","startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize");const k=document.createElementNS("http://www.w3.org/2000/svg","path");return k.setAttribute("d",f),t.properties.strokeColor.contents.tag!=="NONE"&&(k.setAttribute("stroke-opacity",m.toString()),k.setAttribute("stroke-width",w.toString())),k.setAttribute("stroke",h),"strokeDasharray"in t.properties&&t.properties.strokeDasharray.contents!==""?k.setAttribute("stroke-dasharray",t.properties.strokeDasharray.contents):t.properties.strokeStyle.contents==="dashed"&&k.setAttribute("stroke-dasharray",F.toString()),d.push("strokeDasharray","strokeStyle"),"strokeLinecap"in t.properties&&t.properties.strokeLinecap.contents!==""?k.setAttribute("stroke-linecap",t.properties.strokeLinecap.contents):k.setAttribute("stroke-linecap","butt"),d.push("strokeLinecap"),r&&(k.setAttribute("marker-start",`url(#${M})`),d.push("startArrowhead")),e&&(k.setAttribute("marker-end",`url(#${C})`),d.push("endArrowhead")),y.appendChild(k),d.push(...b(t,y)),A(t,y,d),y},"Line"),Gt=c((t,o)=>t.map(r=>{const{cmd:e,contents:s}=r;if(s.length===0&&e!=="Z")return console.error("WARNING: empty path"),"";const i=At.flatten(s.map(n=>{switch(n.tag){case"CoordV":return S(n.contents,o);case"ValueV":return n.contents}})).join(" ");return`${e} ${i}`}).join(" "),"toPathString"),Xt=c(t=>{const o=document.createElementNS("http://www.w3.org/2000/svg","filter");return o.setAttribute("id",t),o.setAttribute("x","0"),o.setAttribute("y","0"),o.setAttribute("width","200%"),o.setAttribute("height","200%"),o.innerHTML=`
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
    `,o},"Shadow"),qt=c(({shape:t,canvasSize:o})=>{const r=t.properties.name.contents+"-startArrowId",e=t.properties.name.contents+"-endArrowId",s=t.properties.name.contents+"-shadow",i=document.createElementNS("http://www.w3.org/2000/svg","g"),n=t.properties.strokeWidth.contents,a=j(t.properties.strokeColor.contents),d=z(t.properties.strokeColor.contents),l=j(t.properties.fillColor.contents),g=z(t.properties.fillColor.contents),p=[],u=T(t.properties.startArrowhead.contents),f=T(t.properties.endArrowhead.contents);if(u){const w=t.properties.name.contents+"-startArrowId",m=t.properties.startArrowheadSize.contents,y=t.properties.flipStartArrowhead.contents;i.appendChild($(w,a,d,u,m,y))}if(f){const w=t.properties.name.contents+"-endArrowId",m=t.properties.endArrowheadSize.contents;i.appendChild($(w,a,d,f,m,!1))}p.push("name","strokeColor","startArrowhead","flipStartArrowhead","endArrowhead"),i.appendChild(Xt(s));const h=document.createElementNS("http://www.w3.org/2000/svg","path");return h.setAttribute("stroke",a),h.setAttribute("fill",l),p.push("fillColor","strokeColor"),t.properties.strokeColor.contents.tag!=="NONE"&&(h.setAttribute("stroke-width",n.toString()),h.setAttribute("stroke-opacity",d.toString()),p.push("strokeColor","strokeWidth")),t.properties.fillColor.contents.tag!=="NONE"&&(h.setAttribute("fill-opacity",g.toString()),p.push("fillColor")),"strokeDasharray"in t.properties&&t.properties.strokeDasharray.contents!==""?h.setAttribute("stroke-dasharray",t.properties.strokeDasharray.contents):t.properties.strokeStyle.contents==="dashed"&&h.setAttribute("stroke-dasharray",F.toString()),p.push("strokeDasharray","strokeStyle"),h.setAttribute("d",Gt(t.properties.d.contents,o)),p.push("d"),u&&(h.setAttribute("marker-start",`url(#${r})`),p.push("startArrowhead")),f&&(h.setAttribute("marker-end",`url(#${e})`),p.push("endArrowhead")),i.appendChild(h),p.push(...b(t,i)),A(t,i,p),i},"Path"),Pt=c(({shape:t,canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","polygon"),e=[];return e.push(...v(t,r)),e.push(...x(t,r)),e.push(...b(t,r)),e.push(...it(t,r)),e.push(...dt(t,o,r)),A(t,r,e),r},"Polygon"),Yt=c(({shape:t,canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","polyline"),e=[];return e.push(...v(t,r)),e.push(...x(t,r)),e.push(...b(t,r)),e.push(...it(t,r)),e.push(...dt(t,o,r)),A(t,r,e),r},"Polyline"),_t=c(({shape:t,canvasSize:o})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","rect"),e=[];return e.push(...jt(t,o,r)),e.push(...O(t,r)),e.push(...v(t,r)),e.push(...x(t,r)),e.push(...b(t,r)),e.push(...Ot(t,r)),e.push(...I(t,o,r)),A(t,r,e),r},"Rectangle"),Bt=c(({shape:t,canvasSize:o,labels:r})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text"),s=[];s.push("x","y"),s.push(...v(t,e)),s.push(...x(t,e)),s.push(...b(t,e)),s.push(...Tt(t,e)),s.push(...I(t,o,e)),s.push(...ct(t,e));const i=t.properties.name,n=r.get(i.contents),a=t.properties.center,[d,l]=S(a.contents,o);if(n&&n.tag==="TextData"){const g=n.descent.contents,p=n.height.contents,u=l+(p/2-g);e.setAttribute("x",d.toString()),e.setAttribute("y",u.toString()),s.push(...O(t,e))}else e.setAttribute("x",d.toString()),e.setAttribute("y",l.toString());return A(t,e,s),e},"Text"),Z={Circle:$t,Ellipse:Rt,Rectangle:_t,Polygon:Pt,Polyline:Yt,Equation:It,Path:qt,Line:Ft,Image:Vt,Text:Bt},lt=c(async({shape:t,labels:o,canvasSize:r,pathResolver:e})=>t.shapeType in Z?await Z[t.shapeType]({shape:t,labels:o,canvasSize:r,pathResolver:e}):(console.error(`${t.shapeType} shape doesn't exist in shapeMap`),document.createElementNS("http://www.w3.org/2000/svg","g")),"RenderShape"),J=c(({clientX:t,clientY:o},r)=>{const e=r.getScreenCTM();return e!==null?{x:(t-e.e)/e.a,y:(o-e.f)/e.d}:{x:0,y:0}},"getPosition"),Ut=c(async(t,o,r,e)=>{const s=t.canvasSize,i=await lt(Object.assign(Object.assign({},t),{canvasSize:e||s})),n=document.createElementNS("http://www.w3.org/2000/svg","g"),{shapeType:a}=t.shape;_[a].isLinelike?n.setAttribute("pointer-events","visibleStroke"):_[a].isRectlike?n.setAttribute("pointer-events","bounding-box"):n.setAttribute("pointer-events","auto"),n.appendChild(i);const d=c(l=>{const{clientX:g,clientY:p}=l,{x:u,y:f}=J({clientX:g,clientY:p},r),{width:h,height:w,x:m,y}=l.target.getBBox({stroke:!0}),M=u-m,C=s[0]-h+(u-m),k=f-y,E=s[1]-w+(f-y);n.setAttribute("opacity","0.5");let L=0,D=0;const X=c(ht=>{const{x:ft,y:gt}=J(ht,r),mt=Q(ft,M,C),yt=Q(gt,k,E);L=mt-u,D=f-yt,n.setAttribute("transform",`translate(${L},${-D})`)},"onMouseMove"),q=c(()=>{n.setAttribute("opacity","1"),document.removeEventListener("mouseup",q),document.removeEventListener("mousemove",X),o(t.shape.properties.name.contents,L,D)},"onMouseUp");document.addEventListener("mouseup",q),document.addEventListener("mousemove",X)},"onMouseDown");return n.addEventListener("mousedown",d),n},"DraggableShape"),Ht=c(async(t,o,r)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","svg");e.setAttribute("xmlns","http://www.w3.org/2000/svg"),e.setAttribute("width","100%"),e.setAttribute("height","100%"),e.setAttribute("version","1.2"),e.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((i,n,a)=>{o(Nt(t,i,n,a))},"onDrag");for(const i of t.computeShapes(t.varyingValues))e.appendChild(await Ut({shape:i,labels:t.labelCache,canvasSize:t.canvas.size,pathResolver:r},s,e));return e},"RenderInteractive"),Kt=c(async(t,o)=>{const{varyingValues:r,computeShapes:e,labelCache:s,canvas:i}=t,n=document.createElementNS("http://www.w3.org/2000/svg","svg");return n.setAttribute("version","1.2"),n.setAttribute("xmlns","http://www.w3.org/2000/svg"),n.setAttribute("viewBox",`0 0 ${i.width} ${i.height}`),Promise.all(e(r).map(a=>lt({shape:a,labels:s,canvasSize:i.size,pathResolver:o}))).then(a=>{for(const d of a)n.appendChild(d);return n})},"RenderStatic"),Q=c((t,o,r)=>Math.min(Math.max(t,o),r),"clamp"),Zt=c(t=>{const o=St(t.variation),{constraintSets:r,optStages:e}=t,{inputMask:s,objMask:i,constrMask:n}=et(r.get(e[0]),"missing first stage");return rt(Object.assign(Object.assign({},t),{varyingValues:t.inputs.map(a=>a.init.tag==="Sampled"?a.init.sampler(o):a.init.pending),currentStageIndex:0,params:W(s,i,n)}))},"resample"),pt=c((t,o=1e4)=>{const r=Object.assign(Object.assign({},t),t.gradient.step(t,o));return R(r)&&!G(r)?ut(r):r},"stepState"),ut=c(t=>{if(G(t))return t;{const{constraintSets:o,optStages:r,currentStageIndex:e}=t,s=r[e+1],{inputMask:i,objMask:n,constrMask:a}=et(o.get(s),"missing next stage");return Object.assign(Object.assign({},t),{currentStageIndex:e+1,params:W(i,n,a)})}},"nextStage"),Jt=c((t,o=1e4)=>{let r=t;for(;r.params.optStatus!=="Error"&&(!R(r)||!G(r));)R(r)&&(r=ut(r)),r=pt(r,o);return r.params.optStatus==="Error"?ot(Object.assign({errorType:"RuntimeError"},bt("",r))):vt(r)},"stepUntilConvergence"),Qt=c(async t=>{const o=xt(t.domain),r=Mt(s=>Ct(t.substance,s),o);return r.isErr()?ot(r.error):await Et(t.variation,t.style,...r.value)},"compileTrio"),te=c(async t=>{const o=await Lt(t.shapes);if(o.isErr())throw Error(st(o.error));return rt(Object.assign(Object.assign({},t),{labelCache:o.value}))},"prepareState"),R=c(t=>t.params.optStatus==="EPConverged","stateConverged"),G=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function tt(t){const o=await fetch(t);if(!o.ok){console.error(`could not fetch ${t}`);return}return await o.text()}c(tt,"fetchResolver");class V extends B.Component{canvasRef=B.createRef();penroseState=void 0;timerID=void 0;constructor(o){super(o),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0;const o=await Qt(this.props);o.isOk()?this.penroseState=await te(o.value):this.setState({error:o.error})};converge=async()=>{if(this.penroseState){const o=Jt(this.penroseState);o.isOk()?this.penroseState=o.value:this.setState({error:o.error})}};tick=()=>{this.props.animate&&this.penroseState&&!R(this.penroseState)&&(this.penroseState=pt(this.penroseState,this.props.stepSize??1),this.props.onFrame&&this.props.onFrame(this.penroseState),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async o=>{(this.props.domain!==o.domain||this.props.substance!==o.substance||this.props.style!==o.style)&&(await this.compile(),this.props.animate||await this.converge(),this.renderCanvas()),this.penroseState&&!this.state.error&&(this.props.variation!==o.variation||this.props.animate!==o.animate?(this.penroseState.variation=this.props.variation,this.penroseState=Zt(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas()):this.props.interactive!==o.interactive&&this.renderCanvas())};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return N("div",{children:"rendering..."});{const o=this.canvasRef.current;if(this.penroseState){const r=await(this.props.interactive===!1?Kt(this.penroseState,tt):Ht(this.penroseState,async e=>{this.penroseState=e,this.props.animate||await this.converge(),this.renderCanvas()},tt));o.firstChild!==null?o.replaceChild(r,o.firstChild):o.appendChild(r)}else console.log("state is undefined")}};render=()=>{const{error:o}=this.state;return U("div",{style:{width:"100%",height:"100%"},children:[!o&&N("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),o&&U("div",{style:{padding:"1em",height:"100%"},children:[N("div",{style:{fontWeight:700},children:"1 error:"}),N("div",{style:{fontFamily:"monospace"},children:st(o).toString().split(`
`).map((r,e)=>N("p",{style:{margin:0},children:r},`err-ln-${e}`))})]})]})}}c(V,"Simple");try{V.displayName="Simple",V.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}}}}}catch{}export{V as S};
//# sourceMappingURL=Simple-d1413a00.js.map
