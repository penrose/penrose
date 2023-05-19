var ut=Object.defineProperty;var c=(t,r)=>ut(t,"name",{value:r,configurable:!0});import{i as J,t as I,a as T,b as C,c as ht,s as q,g as X,d as j,r as Y,_ as ft,e as mt,f as gt,h as yt,j as kt,k as Q,l as tt,n as wt,o as At,m as St,p as vt,q as bt,u as xt,v as Ct,w as et,x as Mt,y as Et}from"./CollectLabels-24bc1788.js";import{R as B}from"./index-74c5fbfa.js";import{j as E,a as U}from"./jsx-runtime-9c5bc5e6.js";const H={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},b=c((t,r,o)=>{const e=["strokeStyle","name","ensureOnCanvas"],n=new Set(o.concat(e));for(const[s,i]of t.passthrough)if(!(i.tag==="StrV"&&i.contents===""||n.has(s)))if(J(s,H)){const a=H[s];r.hasAttribute(a)||r.setAttribute(a,i.contents.toString())}else if(s==="style"&&i.contents!==""){const a=r.getAttribute(s);a===null?r.setAttribute(s,i.contents.toString()):r.setAttribute(s,`${a}${i.contents.toString()}`)}else r.hasAttribute(s)||r.setAttribute(s,i.contents.toString())},"attrAutoFillSvg"),M=c((t,r)=>{const o=t.fillColor,e=I(o.contents);return r.setAttribute("fill",T(o.contents)),o.contents.tag!=="NONE"&&r.setAttribute("fill-opacity",e.toString()),["fillColor"]},"attrFill"),rt=c((t,r,o)=>{const e=t.center,[n,s]=C([e.contents[0],e.contents[1]],r);return o.setAttribute("cx",n.toString()),o.setAttribute("cy",s.toString()),["center"]},"attrCenter"),ot=c((t,r)=>{let o=t.scale.contents;o=o||1;let e=r.getAttribute("transform");return e=e===null?`scale(${o})`:e+`scale{${o}}`,r.setAttribute("transform",e),["scale"]},"attrScale"),nt=c((t,r,o)=>{const e=t.center,[n,s]=C([e.contents[0],e.contents[1]],r),i=t.width,a=t.height;let l=o.getAttribute("transform");return l=l===null?`translate(${n-i.contents/2}, ${s-a.contents/2})`:l+`translate(${n-i.contents/2}, ${s-a.contents/2})`,o.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),Rt=c((t,r,o)=>{const e=t.center,[n,s]=C([e.contents[0],e.contents[1]],r),i=t.width,a=t.height;return o.setAttribute("x",(n-i.contents/2).toString()),o.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),D=c((t,r,o)=>{t.width,t.height;const e=t.center,n=t.rotation.contents,[s,i]=C([e.contents[0],e.contents[1]],r);let a=o.getAttribute("transform");return a=a===null?`rotate(${n}, ${s}, ${i})`:a+`rotate(${n}, ${s}, ${i})`,o.setAttribute("transform",a),["rotation","center","width","height"]},"attrRotation"),$=c((t,r)=>{const o=t.width,e=t.height;return r.setAttribute("width",o.contents.toString()),r.setAttribute("height",e.contents.toString()),["width","height"]},"attrWH"),Lt=c((t,r)=>{const o=t.cornerRadius;return r.setAttribute("rx",o.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Nt=c((t,r)=>{const o=t.string,e=document.createTextNode(o.contents.toString());return r.appendChild(e),["string"]},"attrString"),W="7,5",R=c((t,r)=>{const o=[],e=t.strokeColor,n=I(e.contents),s=t.strokeWidth.contents;return r.setAttribute("stroke",T(e.contents)),o.push("strokeColor","strokeWidth"),e.contents.tag!=="NONE"&&(r.setAttribute("stroke-opacity",n.toString()),r.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?r.setAttribute("stroke-dasharray",t.strokeDasharray.contents):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(r.setAttribute("stroke-dasharray",W.toString()),o.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""?r.setAttribute("stroke-linecap",t.strokeLinecap.contents):r.setAttribute("stroke-linecap","butt"),o.push("strokeLinecap")),o},"attrStroke"),x=c((t,r)=>{const o=t.name,e=document.createElementNS("http://www.w3.org/2000/svg","title");return e.textContent=o.contents,r.appendChild(e),["name"]},"attrTitle"),zt=c((t,r)=>{const o=ht(t),e=r.getAttribute("style");return r.setAttribute("style",e?`${e}; font: ${o};`:`font: ${o};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),it=c((t,r,o)=>{const n=t.points.contents.map(s=>C([s[0],s[1]],r));return o.setAttribute("points",n.toString()),["points"]},"attrPolyPoints"),It=c((t,{canvasSize:r})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","circle"),e=[];return e.push(...M(t,o)),e.push(...rt(t,r,o)),e.push(...R(t,o)),e.push(...x(t,o)),o.setAttribute("r",t.r.contents.toString()),e.push("r"),b(t,o,e),o},"RenderCircle"),Tt=c((t,r,o,e)=>{const n=[...t.varyingValues];return{...t,params:q(n.length),varyingValues:n}},"dragUpdate"),$t=c((t,{canvasSize:r})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),e=[];return e.push(...M(t,o)),e.push(...rt(t,r,o)),e.push(...R(t,o)),e.push(...x(t,o)),o.setAttribute("rx",t.rx.contents.toString()),e.push("rx"),o.setAttribute("ry",t.ry.contents.toString()),e.push("ry"),b(t,o,e),o},"RenderEllipse"),jt=c((t,{canvasSize:r,labels:o})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),n=[];n.push(...D(t,r,e)),n.push(...nt(t,r,e)),n.push(...x(t,e));let s=!1;const i=o.get(X(t.name));if(i&&i.tag==="EquationData"){const a=i.rendered.cloneNode(!0),l=a.getElementsByTagName("g")[0];n.push(...M(t,l)),n.push(...$(t,a)),l.setAttribute("stroke","none"),l.setAttribute("stroke-width","0");const h=t.fontSize;a.setAttribute("style",`font-size: ${h.contents}`),e.appendChild(a),s=!0}if(!s){const a=document.createElementNS("http://www.w3.org/2000/svg","text");a.textContent=X(t.string),n.push("string"),e.appendChild(a),n.push(...M(t,e)),n.push(...$(t,e))}return b(t,e,n),e},"RenderEquation"),Ot=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let Vt=1;const Dt=c((t,r)=>{const o="--inject-",e={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},n=o+Vt++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const l=r?new Set:void 0,h=new Set,d=[];let p=!1,u,k;if(i.length){for(u=0;u<i.length;u++){const f=i[u].localName;J(f,e)&&h.add(f)}h.forEach(f=>{(e[f]||[f]).forEach(function(y){d.indexOf(y)<0&&d.push(y)})}),d.length&&d.push("style");const m=t.getElementsByTagName("*");let g=t,w,A,S;for(u=-1;g!==null;){if(g.localName==="style")A=g.textContent,S=A&&A.replace(s,function(f,y){return l&&l.add(y),"url(#"+y+n+")"}),S!==A&&(g.textContent=S);else if(g.hasAttributes()){for(k=0;k<d.length;k++)w=d[k],A=g.getAttribute(w),S=A&&A.replace(s,function(f,y){return l&&l.add(y),"url(#"+y+n+")"}),S&&S!==A&&g.setAttribute(w,S);for(const f of["xlink:href","href"]){let y=g.getAttribute(f);y&&/^\s*#/.test(y)&&(y=y.trim(),g.setAttribute(f,y+n),l&&l.add(y.substring(1)))}}g=m.item(++u)}for(u=0;u<i.length;u++)a=i[u],(!l||l.has(a.id))&&(a.id+=n,p=!0)}return p},"makeIdsUnique"),Gt=c(async(t,{canvasSize:r,pathResolver:o})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),n=[],s=t.href.contents;let i=await o(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=Ot),n.push("href"),e.innerHTML=i;const a=e.querySelector("svg");return Dt(e,!1),n.push(...$(t,a)),n.push(...D(t,r,e)),n.push(...nt(t,r,e)),n.push(...x(t,e)),b(t,e,n),e},"RenderImage"),O=c((t,r,o,e,n,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",Y(e.width*n).toString()),i.setAttribute("markerHeight",Y(e.height*n).toString()),i.setAttribute("viewBox",e.viewbox),i.setAttribute("refX",e.refX.toString()),i.setAttribute("refY",e.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",e.path),e.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",r),i.setAttribute("stroke-opacity",o.toString())):(a.setAttribute("fill",r),a.setAttribute("fill-opacity",o.toString())),e.style&&Object.entries(e.style).forEach(([l,h])=>{a.setAttribute(l,h)}),i.appendChild(a),i},"arrowHead"),Ft=c((t,r,o)=>{const e=[],[n,s]=[t.start.contents[0],t.start.contents[1]],[i,a]=[t.end.contents[0],t.end.contents[1]],l=t.startArrowheadSize.contents,h=t.endArrowheadSize.contents,d=t.strokeWidth.contents;e.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const p=Math.sqrt((n-i)**2+(s-a)**2);let u,k;if(r){const A=(t.flipStartArrowhead.contents?r.refX:r.width-r.refX)*l*d,S=A/p*(n-i),f=A/p*(s-a);[u,k]=[n-S,s-f]}else[u,k]=[n,s];let m,g;if(o){const w=(o.width-o.refX)*h*d;[m,g]=[i-w/p*(i-n),a-w/p*(a-s)]}else[m,g]=[i,a];return[[[u,k],[m,g]],e]},"makeRoomForArrows"),qt=c((t,{canvasSize:r,namespace:o,variation:e})=>{const n=j(t.startArrowhead.contents),s=j(t.endArrowhead.contents),[[[i,a],[l,h]],d]=Ft(t,n,s),[p,u]=C([i,a],r),[k,m]=C([l,h],r),g=`M ${p} ${u} L ${k} ${m}`,w=T(t.strokeColor.contents),A=t.strokeWidth.contents,S=I(t.strokeColor.contents),f=document.createElementNS("http://www.w3.org/2000/svg","g"),y=`${o}-${e}-${t.name.contents}`,L=y+"-startArrowId",N=y+"-endArrowId";if(n){const z=t.startArrowheadSize.contents,G=t.flipStartArrowhead.contents;f.appendChild(O(L,w,S,n,z,G))}if(s){const z=t.endArrowheadSize.contents;f.appendChild(O(N,w,S,s,z,!1))}d.push("strokeColor","strokeWidth","startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize");const v=document.createElementNS("http://www.w3.org/2000/svg","path");return v.setAttribute("d",g),t.strokeColor.contents.tag!=="NONE"&&(v.setAttribute("stroke-opacity",S.toString()),v.setAttribute("stroke-width",A.toString())),v.setAttribute("stroke",w),t.strokeDasharray.contents!==""?v.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&v.setAttribute("stroke-dasharray",W.toString()),d.push("strokeDasharray","strokeStyle"),t.strokeLinecap.contents!==""?v.setAttribute("stroke-linecap",t.strokeLinecap.contents):v.setAttribute("stroke-linecap","butt"),d.push("strokeLinecap"),n&&(v.setAttribute("marker-start",`url(#${L})`),d.push("startArrowhead")),s&&(v.setAttribute("marker-end",`url(#${N})`),d.push("endArrowhead")),f.appendChild(v),d.push(...x(t,f)),b(t,f,d),f},"RenderLine"),Wt=c((t,r)=>t.map(o=>{const{cmd:e,contents:n}=o;if(n.length===0&&e!=="Z")return console.error("WARNING: empty path"),"";const s=ft.flatten(n.map(i=>{switch(i.tag){case"CoordV":return C([i.contents[0],i.contents[1]],r);case"ValueV":return i.contents}})).join(" ");return`${e} ${s}`}).join(" "),"toPathString"),_t=c(t=>{const r=document.createElementNS("http://www.w3.org/2000/svg","filter");return r.setAttribute("id",t),r.setAttribute("x","0"),r.setAttribute("y","0"),r.setAttribute("width","200%"),r.setAttribute("height","200%"),r.innerHTML=`
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
    `,r},"Shadow"),Xt=c((t,{canvasSize:r})=>{const o=t.name.contents+"-startArrowId",e=t.name.contents+"-endArrowId",n=t.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=t.strokeWidth.contents,a=T(t.strokeColor.contents),l=I(t.strokeColor.contents),h=T(t.fillColor.contents),d=I(t.fillColor.contents),p=[],u=j(t.startArrowhead.contents),k=j(t.endArrowhead.contents);if(u){const g=t.name.contents+"-startArrowId",w=t.startArrowheadSize.contents,A=t.flipStartArrowhead.contents;s.appendChild(O(g,a,l,u,w,A))}if(k){const g=t.name.contents+"-endArrowId",w=t.endArrowheadSize.contents;s.appendChild(O(g,a,l,k,w,!1))}p.push("name","strokeColor","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(_t(n));const m=document.createElementNS("http://www.w3.org/2000/svg","path");return m.setAttribute("stroke",a),m.setAttribute("fill",h),p.push("fillColor","strokeColor"),t.strokeColor.contents.tag!=="NONE"&&(m.setAttribute("stroke-width",i.toString()),m.setAttribute("stroke-opacity",l.toString()),p.push("strokeColor","strokeWidth")),t.fillColor.contents.tag!=="NONE"&&(m.setAttribute("fill-opacity",d.toString()),p.push("fillColor")),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?m.setAttribute("stroke-dasharray",t.strokeDasharray.contents):t.strokeStyle.contents==="dashed"&&m.setAttribute("stroke-dasharray",W.toString()),p.push("strokeDasharray","strokeStyle"),m.setAttribute("d",Wt(t.d.contents,r)),p.push("d"),u&&(m.setAttribute("marker-start",`url(#${o})`),p.push("startArrowhead")),k&&(m.setAttribute("marker-end",`url(#${e})`),p.push("endArrowhead")),s.appendChild(m),p.push(...x(t,s)),b(t,s,p),s},"RenderPath"),Yt=c((t,{canvasSize:r})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","polygon"),e=[];return e.push(...M(t,o)),e.push(...R(t,o)),e.push(...x(t,o)),e.push(...ot(t,o)),e.push(...it(t,r,o)),b(t,o,e),o},"RenderPolygon"),Bt=c((t,{canvasSize:r})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","polyline"),e=[];return e.push(...M(t,o)),e.push(...R(t,o)),e.push(...x(t,o)),e.push(...ot(t,o)),e.push(...it(t,r,o)),b(t,o,e),o},"RenderPolyline"),Ut=c((t,{canvasSize:r})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","rect"),e=[];return e.push(...Rt(t,r,o)),e.push(...$(t,o)),e.push(...M(t,o)),e.push(...R(t,o)),e.push(...x(t,o)),e.push(...Lt(t,o)),e.push(...D(t,r,o)),b(t,o,e),o},"RenderRectangle"),Ht=c((t,{canvasSize:r,labels:o})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","text"),n=[];n.push("x","y"),n.push(...M(t,e)),n.push(...R(t,e)),n.push(...x(t,e)),n.push(...Nt(t,e)),n.push(...D(t,r,e)),n.push(...zt(t,e));const s=t.name,i=o.get(s.contents),a=t.center,[l,h]=C([a.contents[0],a.contents[1]],r);if(i&&i.tag==="TextData"){const d=i.descent.contents,p=i.height.contents,u=h+(p/2-d);e.setAttribute("x",l.toString()),e.setAttribute("y",u.toString()),n.push(...$(t,e))}else e.setAttribute("x",l.toString()),e.setAttribute("y",h.toString());return e.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),e.setAttribute("alignment-baseline",t.alignmentBaseline.contents),e.setAttribute("dominant-baseline",t.dominantBaseline.contents),e.setAttribute("ascent",t.ascent.contents.toString()),e.setAttribute("descent",t.descent.contents.toString()),e.setAttribute("text-anchor",t.textAnchor.contents.toString()),e.setAttribute("visibility",t.visibility.contents),n.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),b(t,e,n),e},"RenderText"),P=c(({clientX:t,clientY:r},o)=>{const e=o.getScreenCTM();return e!==null?{x:(t-e.e)/e.a,y:(r-e.f)/e.d}:{x:0,y:0}},"getPosition"),Pt=c(async(t,r,o,e)=>{const n=document.createElementNS("http://www.w3.org/2000/svg","svg");n.setAttribute("xmlns","http://www.w3.org/2000/svg"),n.setAttribute("width","100%"),n.setAttribute("height","100%"),n.setAttribute("version","1.2"),n.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((a,l,h)=>{r(Tt(t))},"onDrag"),i=t.computeShapes(t.varyingValues);return await at(i,n,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:e,pathResolver:o},{updateState:r,onDrag:s,parentSVG:n}),n},"RenderInteractive"),Kt=c(async(t,r,o)=>{const{varyingValues:e,computeShapes:n,labelCache:s,canvas:i,variation:a}=t,l=document.createElementNS("http://www.w3.org/2000/svg","svg");l.setAttribute("version","1.2"),l.setAttribute("xmlns","http://www.w3.org/2000/svg"),l.setAttribute("viewBox",`0 0 ${i.width} ${i.height}`);const h=n(e);return await at(h,l,{labels:s,canvasSize:i.size,variation:a,namespace:o,pathResolver:r},void 0),l},"RenderStatic"),Zt=c(async(t,r,o)=>{const e=document.createElementNS("http://www.w3.org/2000/svg","g"),n=yt(t.shapes);for(const s of n){const i=await st(s,r,o);e.appendChild(i)}return b(t,e,[...x(t,e),"shapes"]),e},"RenderGroup"),Jt=c(async(t,r)=>{switch(t.shapeType){case"Circle":return It(t,r);case"Ellipse":return $t(t,r);case"Equation":return jt(t,r);case"Image":return Gt(t,r);case"Line":return qt(t,r);case"Path":return Xt(t,r);case"Polygon":return Yt(t,r);case"Polyline":return Bt(t,r);case"Rectangle":return Ut(t,r);case"Text":return Ht(t,r)}},"RenderShapeSvg"),st=c(async(t,r,o)=>{if(t.shapeType==="Group")return await Zt(t,r,o);{const e=await Jt(t,r);if(o){const n=document.createElementNS("http://www.w3.org/2000/svg","g");mt(t)?n.setAttribute("pointer-events","visibleStroke"):gt(t)?n.setAttribute("pointer-events","bounding-box"):n.setAttribute("pointer-events","auto"),n.appendChild(e);const s=c(i=>{const{clientX:a,clientY:l}=i,{x:h,y:d}=P({clientX:a,clientY:l},o.parentSVG),{width:p,height:u,x:k,y:m}=i.target.getBBox({stroke:!0}),g=h-k,w=r.canvasSize[0]-p+(h-k),A=d-m,S=r.canvasSize[1]-u+(d-m);n.setAttribute("opacity","0.5");let f=0,y=0;const L=c(v=>{const{x:z,y:G}=P(v,o.parentSVG),dt=K(z,g,w),pt=K(G,A,S);f=dt-h,y=d-pt,n.setAttribute("transform",`translate(${f},${-y})`)},"onMouseMove"),N=c(()=>{n.setAttribute("opacity","1"),document.removeEventListener("mouseup",N),document.removeEventListener("mousemove",L),o.onDrag(t.name.contents,f,y)},"onMouseUp");document.addEventListener("mouseup",N),document.addEventListener("mousemove",L)},"onMouseDown");return n.addEventListener("mousedown",s),n}else return e}},"RenderShape"),at=c(async(t,r,o,e)=>{for(const n of t){const s=await st(n,o,e);r.appendChild(s)}},"RenderShapes"),K=c((t,r,o)=>Math.min(Math.max(t,r),o),"clamp"),Qt=c(t=>{const r=kt(t.variation);return Q({...t,varyingValues:t.inputs.map(({meta:o})=>o.init.tag==="Sampled"?o.init.sampler(r):o.init.pending),currentStageIndex:0,params:q(t.varyingValues.length)})},"resample"),te=c((t,r)=>{const{constraintSets:o,optStages:e,currentStageIndex:n}=t,s=e[n],i=Mt(o.get(s),"missing stage"),a=new Float64Array(t.varyingValues);let l=0;const h=Et((d,p,u)=>t.gradient(i,d,p,u).phi,a,t.params,()=>l++>=r);return{...t,varyingValues:Array.from(a),params:h}},"step"),ct=c((t,r=1e4)=>{const o=te(t,r);return V(o)&&!_(o)?lt(o):o},"stepState"),lt=c(t=>_(t)?t:{...t,currentStageIndex:t.currentStageIndex+1,params:q(t.varyingValues.length)},"nextStage"),ee=c((t,r=1e4)=>{let o=t;for(;o.params.optStatus!=="Error"&&(!V(o)||!_(o));)V(o)&&(o=lt(o)),o=ct(o,r);return o.params.optStatus==="Error"?tt({errorType:"RuntimeError",...wt("",o)}):At(o)},"stepUntilConvergence"),re=c(async t=>{const r=St(t.domain),o=vt(n=>bt(t.substance,n),r);return o.isErr()?tt(o.error):await xt(t.variation,t.style,...o.value)},"compileTrio"),oe=c(async t=>{const r=await Ct(t.shapes);if(r.isErr())throw Error(et(r.error));return Q({...t,labelCache:r.value})},"prepareState"),V=c(t=>t.params.optStatus==="EPConverged","stateConverged"),_=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function Z(t){const r=await fetch(t);if(!r.ok){console.error(`could not fetch ${t}`);return}return await r.text()}c(Z,"fetchResolver");class F extends B.Component{canvasRef=B.createRef();penroseState=void 0;timerID=void 0;constructor(r){super(r),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const r=await re(this.props);r.isOk()?(this.penroseState=await oe(r.value),this.setState({error:void 0})):this.setState({error:r.error})};converge=async()=>{if(this.penroseState){const r=ee(this.penroseState);r.isOk()?this.penroseState=r.value:this.setState({error:r.error})}};tick=()=>{this.props.animate&&this.penroseState&&!V(this.penroseState)&&(this.penroseState=ct(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async r=>{if(this.props.domain!==r.domain||this.props.substance!==r.substance||this.props.style!==r.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==r.variation||this.props.animate!==r.animate){this.penroseState.variation=this.props.variation,this.penroseState=Qt(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==r.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return E("div",{children:"rendering..."});{const r=this.canvasRef.current;if(this.penroseState){const o=await(this.props.interactive===!1?Kt(this.penroseState,this.props.imageResolver??Z,this.props.name??""):Pt(this.penroseState,async e=>{this.penroseState=e,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??Z,this.props.name??""));o.setAttribute("height","100%"),o.setAttribute("width","100%"),r.firstChild!==null?r.replaceChild(o,r.firstChild):r.appendChild(o),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return E("div",{children:"rendering..."})}};render=()=>{const{error:r}=this.state;return U("div",{style:{width:"100%",height:"100%"},children:[!r&&E("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),r&&U("div",{style:{padding:"1em",height:"100%"},children:[E("div",{style:{fontWeight:700},children:"1 error:"}),E("div",{style:{fontFamily:"monospace"},children:et(r).toString().split(`
`).map((o,e)=>E("p",{style:{margin:0},children:o},`err-ln-${e}`))})]})]})}}c(F,"Simple");try{F.displayName="Simple",F.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{F as S};
//# sourceMappingURL=Simple-ad809e56.js.map
