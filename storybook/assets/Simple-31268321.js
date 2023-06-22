var gt=Object.defineProperty;var c=(t,n)=>gt(t,"name",{value:n,configurable:!0});import{i as it,t as q,a as Y,b as v,g as G,d as D,r as K,_ as yt,s as U,e as wt,m as Z,f as kt,h as At,j as J,k as St,n as vt,o as bt,p as xt,q as Mt,u as Ct,v as Et,w as st,x as Rt,y as Nt,c as Lt,A as zt,B as $t,C as It,D as at,E as Tt,F as jt}from"./svg-dbb164f9.js";import{t as Vt,i as ct,m as Gt,c as Ot}from"./CollectLabels-07e4a1e0.js";import{R as Q,j as L,a as tt}from"./jsx-runtime-728b5a79.js";const et={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},x=c((t,n,e)=>{const o=["strokeStyle","name","ensureOnCanvas"],r=new Set(e.concat(o));for(const[s,i]of t.passthrough)if(!(i.tag==="StrV"&&i.contents===""||r.has(s)))if(it(s,et)){const a=et[s];n.hasAttribute(a)||n.setAttribute(a,i.contents.toString())}else if(s==="style"&&i.contents!==""){const a=n.getAttribute(s);a===null?n.setAttribute(s,i.contents.toString()):n.setAttribute(s,`${a}${i.contents.toString()}`)}else n.hasAttribute(s)||n.setAttribute(s,i.contents.toString())},"attrAutoFillSvg"),C=c((t,n)=>{const e=t.fillColor,o=q(e.contents);return n.setAttribute("fill",Y(e.contents)),e.contents.tag!=="NONE"&&n.setAttribute("fill-opacity",o.toString()),["fillColor"]},"attrFill"),dt=c((t,n,e)=>{const o=t.center,[r,s]=v([o.contents[0],o.contents[1]],n);return e.setAttribute("cx",r.toString()),e.setAttribute("cy",s.toString()),["center"]},"attrCenter"),lt=c((t,n)=>{let e=t.scale.contents;e=e||1;let o=n.getAttribute("transform");return o=o===null?`scale(${e})`:o+`scale{${e}}`,n.setAttribute("transform",o),["scale"]},"attrScale"),pt=c((t,n,e)=>{const o=t.center,[r,s]=v([o.contents[0],o.contents[1]],n),i=t.width,a=t.height;let d=e.getAttribute("transform");return d=d===null?`translate(${r-i.contents/2}, ${s-a.contents/2})`:d+`translate(${r-i.contents/2}, ${s-a.contents/2})`,e.setAttribute("transform",d),["center","width","height"]},"attrTransformCoords"),Dt=c((t,n,e)=>{const o=t.center,[r,s]=v([o.contents[0],o.contents[1]],n),i=t.width,a=t.height;return e.setAttribute("x",(r-i.contents/2).toString()),e.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),B=c((t,n,e)=>{t.width,t.height;const o=t.center,r=t.rotation.contents,[s,i]=v([o.contents[0],o.contents[1]],n);let a=e.getAttribute("transform");return a=a===null?`rotate(${r}, ${s}, ${i})`:a+`rotate(${r}, ${s}, ${i})`,e.setAttribute("transform",a),["rotation","center","width","height"]},"attrRotation"),$=c((t,n)=>{const e=t.width,o=t.height;return n.setAttribute("width",e.contents.toString()),n.setAttribute("height",o.contents.toString()),["width","height"]},"attrWH"),Ft=c((t,n)=>{const e=t.cornerRadius;return n.setAttribute("rx",e.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Xt=c((t,n)=>{const e=t.string,o=document.createTextNode(e.contents.toString());return n.appendChild(o),["string"]},"attrString"),qt="7,5",N=c((t,n)=>{const e=[],o=t.strokeColor,r=q(o.contents),s=t.strokeWidth.contents;return n.setAttribute("stroke",Y(o.contents)),e.push("strokeColor","strokeWidth"),o.contents.tag!=="NONE"&&(n.setAttribute("stroke-opacity",r.toString()),n.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?(n.setAttribute("stroke-dasharray",t.strokeDasharray.contents),e.push("strokeDasharray")):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(n.setAttribute("stroke-dasharray",qt.toString()),e.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""&&(n.setAttribute("stroke-linecap",t.strokeLinecap.contents),e.push("strokeLinecap"))),e},"attrStroke"),M=c((t,n)=>{const e=t.name,o=document.createElementNS("http://www.w3.org/2000/svg","title");return o.textContent=e.contents,n.appendChild(o),["name"]},"attrTitle"),Yt=c((t,n)=>{const e=Vt(t),o=n.getAttribute("style");return n.setAttribute("style",o?`${o}; font: ${e};`:`font: ${e};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),ut=c((t,n,e)=>{const r=t.points.contents.map(s=>v([s[0],s[1]],n));return e.setAttribute("points",r.toString()),["points"]},"attrPolyPoints"),Bt=c((t,{canvasSize:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","circle"),o=[];return o.push(...C(t,e)),o.push(...dt(t,n,e)),o.push(...N(t,e)),o.push(...M(t,e)),e.setAttribute("r",t.r.contents.toString()),o.push("r"),x(t,e,o),e},"RenderCircle"),_t=c((t,{canvasSize:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),o=[];return o.push(...C(t,e)),o.push(...dt(t,n,e)),o.push(...N(t,e)),o.push(...M(t,e)),e.setAttribute("rx",t.rx.contents.toString()),o.push("rx"),e.setAttribute("ry",t.ry.contents.toString()),o.push("ry"),x(t,e,o),e},"RenderEllipse"),_=c((t,[n,e],o)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","text");return r.textContent=t,C(o,r),$(o,r),r.setAttribute("x",`${n}`),r.setAttribute("y",`${e}`),r.setAttribute("alignment-baseline","alphabetic"),r.setAttribute("dominant-baseline","alphabetic"),r.setAttribute("text-anchor","middle"),r},"placeholderString"),Wt=c((t,n)=>{const{canvasSize:e,labels:o,texLabels:r}=n,{center:s}=t,[i,a]=v([s.contents[0],s.contents[1]],e);if(r){const u=a+t.height.contents/2-t.descent.contents;let h=_(`$${G(t.string)}$`,[i,u],t);for(const[f,m]of t.passthrough)f==="texContourColor"&&m.contents!==""&&(h=_(`\\contour{${m.contents}}{$${G(t.string)}$}`,[i,u],t));return h}const d=document.createElementNS("http://www.w3.org/2000/svg","g"),p=[];p.push(...B(t,e,d)),p.push(...pt(t,e,d)),p.push(...M(t,d));const l=o.get(G(t.name));if(l&&l.tag==="EquationData"){const u=l.rendered.cloneNode(!0),h=u.getElementsByTagName("g")[0];p.push(...C(t,h)),p.push(...$(t,u)),h.setAttribute("stroke","none"),h.setAttribute("stroke-width","0");const f=t.fontSize;return u.setAttribute("style",`font-size: ${f.contents}`),d.appendChild(u),x(t,d,p),d}else return _(G(t.string),[i,a],t)},"RenderEquation"),Ut=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let Ht=1;const Pt=c((t,n)=>{const e="--inject-",o={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},r=e+Ht++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const d=n?new Set:void 0,p=new Set,l=[];let u=!1,h,f;if(i.length){for(h=0;h<i.length;h++){const y=i[h].localName;it(y,o)&&p.add(y)}p.forEach(y=>{(o[y]||[y]).forEach(function(g){l.indexOf(g)<0&&l.push(g)})}),l.length&&l.push("style");const m=t.getElementsByTagName("*");let w=t,S,k,A;for(h=-1;w!==null;){if(w.localName==="style")k=w.textContent,A=k&&k.replace(s,function(y,g){return d&&d.add(g),"url(#"+g+r+")"}),A!==k&&(w.textContent=A);else if(w.hasAttributes()){for(f=0;f<l.length;f++)S=l[f],k=w.getAttribute(S),A=k&&k.replace(s,function(y,g){return d&&d.add(g),"url(#"+g+r+")"}),A&&A!==k&&w.setAttribute(S,A);for(const y of["xlink:href","href"]){let g=w.getAttribute(y);g&&/^\s*#/.test(g)&&(g=g.trim(),w.setAttribute(y,g+r),d&&d.add(g.substring(1)))}}w=m.item(++h)}for(h=0;h<i.length;h++)a=i[h],(!d||d.has(a.id))&&(a.id+=r,u=!0)}return u},"makeIdsUnique"),Kt=c(async(t,{canvasSize:n,pathResolver:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","g"),r=[],s=t.href.contents;let i=await e(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=Ut),r.push("href"),o.innerHTML=i;const a=o.querySelector("svg");return Pt(o,!1),r.push(...$(t,a)),r.push(...B(t,n,o)),r.push(...pt(t,n,o)),r.push(...M(t,o)),x(t,o,r),o},"RenderImage"),F=c((t,n,e,o,r,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",K(o.width*r).toString()),i.setAttribute("markerHeight",K(o.height*r).toString()),i.setAttribute("viewBox",o.viewbox),i.setAttribute("refX",o.refX.toString()),i.setAttribute("refY",o.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",o.path),o.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",n),i.setAttribute("stroke-opacity",e.toString())):(a.setAttribute("fill",n),a.setAttribute("fill-opacity",e.toString())),o.style&&Object.entries(o.style).forEach(([d,p])=>{a.setAttribute(d,p)}),i.appendChild(a),i},"arrowHead"),Zt=c((t,n,e)=>{const o=[],[r,s]=[t.start.contents[0],t.start.contents[1]],[i,a]=[t.end.contents[0],t.end.contents[1]],d=t.startArrowheadSize.contents,p=t.endArrowheadSize.contents,l=t.strokeWidth.contents;o.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const u=Math.sqrt((r-i)**2+(s-a)**2);if(u===0)return[[[r,s],[i,a]],o];let h,f;if(n){const k=(t.flipStartArrowhead.contents?n.refX:n.width-n.refX)*d*l,A=k/u*(r-i),y=k/u*(s-a);[h,f]=[r-A,s-y]}else[h,f]=[r,s];let m,w;if(e){const S=(e.width-e.refX)*p*l;[m,w]=[i-S/u*(i-r),a-S/u*(a-s)]}else[m,w]=[i,a];return[[[h,f],[m,w]],o]},"makeRoomForArrows"),Jt=c((t,{canvasSize:n,namespace:e,variation:o})=>{const r=D(t.startArrowhead.contents),s=D(t.endArrowhead.contents),[[[i,a],[d,p]],l]=Zt(t,r,s),u=Y(t.strokeColor.contents),h=q(t.strokeColor.contents),f=document.createElementNS("http://www.w3.org/2000/svg","g"),m=document.createElementNS("http://www.w3.org/2000/svg","line"),[w,S]=v([i,a],n),[k,A]=v([d,p],n);m.setAttribute("x1",w.toString()),m.setAttribute("y1",S.toString()),m.setAttribute("x2",k.toString()),m.setAttribute("y2",A.toString());const y=`${e}-${o}-${t.name.contents}`,g=y+"-startArrowId",E=y+"-endArrowId";if(r){const R=t.startArrowheadSize.contents,z=t.flipStartArrowhead.contents;f.appendChild(F(g,u,h,r,R,z))}if(s){const R=t.endArrowheadSize.contents;f.appendChild(F(E,u,h,s,R,!1))}return l.push("startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize"),l.push(...N(t,m)),r&&(m.setAttribute("marker-start",`url(#${g})`),l.push("startArrowhead")),s&&(m.setAttribute("marker-end",`url(#${E})`),l.push("endArrowhead")),f.appendChild(m),l.push(...M(t,f)),x(t,f,l),f},"RenderLine"),Qt=c((t,n)=>t.map(e=>{const{cmd:o,contents:r}=e;if(r.length===0&&o!=="Z")return console.error("WARNING: empty path"),"";const s=yt.flatten(r.map(i=>{switch(i.tag){case"CoordV":return v([i.contents[0],i.contents[1]],n);case"ValueV":return i.contents}})).join(" ");return`${o} ${s}`}).join(" "),"toPathString"),te=c(t=>{const n=document.createElementNS("http://www.w3.org/2000/svg","filter");return n.setAttribute("id",t),n.setAttribute("x","0"),n.setAttribute("y","0"),n.setAttribute("width","200%"),n.setAttribute("height","200%"),n.innerHTML=`
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
    `,n},"Shadow"),ee=c((t,{canvasSize:n})=>{const e=t.name.contents+"-startArrowId",o=t.name.contents+"-endArrowId",r=t.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=Y(t.strokeColor.contents),a=q(t.strokeColor.contents),d=[],p=D(t.startArrowhead.contents),l=D(t.endArrowhead.contents);if(p){const h=t.name.contents+"-startArrowId",f=t.startArrowheadSize.contents,m=t.flipStartArrowhead.contents;s.appendChild(F(h,i,a,p,f,m))}if(l){const h=t.name.contents+"-endArrowId",f=t.endArrowheadSize.contents;s.appendChild(F(h,i,a,l,f,!1))}d.push("name","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(te(r));const u=document.createElementNS("http://www.w3.org/2000/svg","path");return d.push(...C(t,u)),d.push(...N(t,u)),u.setAttribute("d",Qt(t.d.contents,n)),d.push("d"),p&&(u.setAttribute("marker-start",`url(#${e})`),d.push("startArrowhead")),l&&(u.setAttribute("marker-end",`url(#${o})`),d.push("endArrowhead")),s.appendChild(u),d.push(...M(t,s)),x(t,s,d),s},"RenderPath"),ne=c((t,{canvasSize:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","polygon"),o=[];return o.push(...C(t,e)),o.push(...N(t,e)),o.push(...M(t,e)),o.push(...lt(t,e)),o.push(...ut(t,n,e)),x(t,e,o),e},"RenderPolygon"),oe=c((t,{canvasSize:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","polyline"),o=[];return o.push(...C(t,e)),o.push(...N(t,e)),o.push(...M(t,e)),o.push(...lt(t,e)),o.push(...ut(t,n,e)),x(t,e,o),e},"RenderPolyline"),re=c((t,{canvasSize:n})=>{const e=document.createElementNS("http://www.w3.org/2000/svg","rect"),o=[];return o.push(...Dt(t,n,e)),o.push(...$(t,e)),o.push(...C(t,e)),o.push(...N(t,e)),o.push(...M(t,e)),o.push(...Ft(t,e)),o.push(...B(t,n,e)),x(t,e,o),e},"RenderRectangle"),ie=c((t,{canvasSize:n,labels:e})=>{const o=document.createElementNS("http://www.w3.org/2000/svg","text"),r=[];r.push("x","y"),r.push(...C(t,o)),r.push(...N(t,o)),r.push(...M(t,o)),r.push(...Xt(t,o)),r.push(...B(t,n,o)),r.push(...Yt(t,o));const s=t.name,i=e.get(s.contents),a=t.center,[d,p]=v([a.contents[0],a.contents[1]],n);if(i&&i.tag==="TextData"){const l=i.descent.contents,u=i.height.contents,h=p+(u/2-l);o.setAttribute("x",d.toString()),o.setAttribute("y",h.toString()),r.push(...$(t,o))}else o.setAttribute("x",d.toString()),o.setAttribute("y",p.toString());return o.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),o.setAttribute("alignment-baseline",t.alignmentBaseline.contents),o.setAttribute("dominant-baseline",t.dominantBaseline.contents),o.setAttribute("ascent",t.ascent.contents.toString()),o.setAttribute("descent",t.descent.contents.toString()),o.setAttribute("text-anchor",t.textAnchor.contents.toString()),o.setAttribute("visibility",t.visibility.contents),r.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),x(t,o,r),o},"RenderText"),se=c((t,n,e,o)=>{const r=[...t.varyingValues];return{...t,params:U(r.length),varyingValues:r}},"dragUpdate"),nt=c(({clientX:t,clientY:n},e)=>{const o=e.getScreenCTM();return o!==null?{x:(t-o.e)/o.a,y:(n-o.f)/o.d}:{x:0,y:0}},"getPosition"),ae=c(async(t,n,e,o)=>{const r=document.createElementNS("http://www.w3.org/2000/svg","svg");r.setAttribute("xmlns","http://www.w3.org/2000/svg"),r.setAttribute("version","1.2"),r.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((a,d,p)=>{n(se(t))},"onDrag"),i=t.computeShapes(t.varyingValues);return await ht(i,r,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:o,texLabels:!1,pathResolver:e},{updateState:n,onDrag:s,parentSVG:r}),r},"RenderInteractive"),ce=c(async(t,n,e,o=!1)=>{const{varyingValues:r,computeShapes:s,labelCache:i,canvas:a,variation:d}=t,p=document.createElementNS("http://www.w3.org/2000/svg","svg");p.setAttribute("version","1.2"),p.setAttribute("xmlns","http://www.w3.org/2000/svg"),p.setAttribute("viewBox",`0 0 ${a.width} ${a.height}`);const l=s(r),u=l.map(b=>wt(b)),h=Z(u.map(b=>kt(b))),f=Z(u.map(b=>At(b))),m=J(u.map(b=>St(b))),w=J(u.map(b=>vt(b))),S=[h,f,m,w],[k,A,y,g]=(await bt(xt(S)))(b=>b.val).secondary,[E,R]=v([k,A],[a.width,a.height]),[z,I]=v([y,g],[a.width,a.height]),T=[E,I],j=[z-E,R-I];p.setAttribute("penrose","0");const V=document.createElementNS("https://penrose.cs.cmu.edu/metadata","penrose"),P=document.createElementNS("https://penrose.cs.cmu.edu/croppedViewBox","croppedViewBox");return P.insertAdjacentText("afterbegin",`${T[0]} ${T[1]} ${j[0]} ${j[1]}`),V.appendChild(P),p.appendChild(V),await ht(l,p,{labels:i,canvasSize:a.size,variation:d,namespace:e,texLabels:o,pathResolver:n},void 0),p},"RenderStatic"),de=c(async(t,n,e)=>{const o=document.createElementNS("http://www.w3.org/2000/svg","g"),r=t.clipPath.contents;let s,i;if(r.tag==="Clip"){const d=r.contents;s=d.name.contents;const p=await O(d,n,e),l=document.createElementNS("http://www.w3.org/2000/svg","clipPath");i=n.namespace+s+"-clip",l.setAttribute("id",i),l.appendChild(p),o.appendChild(l)}const a=t.shapes.contents;for(const d of a){const p=d.name.contents;if(r.tag==="Clip"){if(p!==s){const l=await O(d,n,e),u=document.createElementNS("http://www.w3.org/2000/svg","g");u.appendChild(l),u.setAttribute("clip-path",`url(#${i})`),o.appendChild(u)}}else{const l=await O(d,n,e);o.appendChild(l)}}return x(t,o,[...M(t,o),"shapes","clipPath"]),o},"RenderGroup"),le=c(async(t,n)=>{switch(t.shapeType){case"Circle":return Bt(t,n);case"Ellipse":return _t(t,n);case"Equation":return Wt(t,n);case"Image":return Kt(t,n);case"Line":return Jt(t,n);case"Path":return ee(t,n);case"Polygon":return ne(t,n);case"Polyline":return oe(t,n);case"Rectangle":return re(t,n);case"Text":return ie(t,n)}},"RenderShapeSvg"),O=c(async(t,n,e)=>{if(t.shapeType==="Group")return await de(t,n,e);{const o=await le(t,n);if(e){const r=document.createElementNS("http://www.w3.org/2000/svg","g");Mt(t)?r.setAttribute("pointer-events","visibleStroke"):Ct(t)?r.setAttribute("pointer-events","bounding-box"):r.setAttribute("pointer-events","auto"),r.appendChild(o);const s=c(i=>{const{clientX:a,clientY:d}=i,{x:p,y:l}=nt({clientX:a,clientY:d},e.parentSVG),{width:u,height:h,x:f,y:m}=i.target.getBBox({stroke:!0}),w=p-f,S=n.canvasSize[0]-u+(p-f),k=l-m,A=n.canvasSize[1]-h+(l-m);r.setAttribute("opacity","0.5");let y=0,g=0;const E=c(z=>{const{x:I,y:T}=nt(z,e.parentSVG),j=ot(I,w,S),V=ot(T,k,A);y=j-p,g=l-V,r.setAttribute("transform",`translate(${y},${-g})`)},"onMouseMove"),R=c(()=>{r.setAttribute("opacity","1"),document.removeEventListener("mouseup",R),document.removeEventListener("mousemove",E),e.onDrag(t.name.contents,y,g)},"onMouseUp");document.addEventListener("mouseup",R),document.addEventListener("mousemove",E)},"onMouseDown");return r.addEventListener("mousedown",s),r}else return o}},"RenderShape"),ht=c(async(t,n,e,o)=>{for(const r of t){const s=await O(r,e,o);n.appendChild(s)}},"RenderShapes"),ot=c((t,n,e)=>Math.min(Math.max(t,n),e),"clamp"),pe=c(t=>{const n=Et(t.variation);return ct({...t,varyingValues:t.inputs.map(({meta:e})=>e.init.tag==="Sampled"?e.init.sampler(n):e.init.pending),currentStageIndex:0,params:U(t.varyingValues.length)})},"resample"),ue=c((t,n)=>{const{constraintSets:e,optStages:o,currentStageIndex:r}=t,s=o[r],i=Tt(e.get(s),"missing stage"),a=new Float64Array(t.varyingValues);let d=0;const p=jt((l,u,h)=>t.gradient(i,l,u,h).phi,a,t.params,()=>d++>=n);return{...t,varyingValues:Array.from(a),params:p}},"step"),ft=c((t,n=1e4)=>{const e=ue(t,n);return X(e)&&!H(e)?mt(e):e},"stepState"),mt=c(t=>H(t)?t:{...t,currentStageIndex:t.currentStageIndex+1,params:U(t.varyingValues.length)},"nextStage"),he=c((t,n=1e4)=>{let e=t;for(;e.params.optStatus!=="Error"&&(!X(e)||!H(e));)X(e)&&(e=mt(e)),e=ft(e,n);return e.params.optStatus==="Error"?st({errorType:"RuntimeError",...Rt("",e)}):Nt(e)},"stepUntilConvergence"),fe=c(async t=>{const n=Lt(t.domain),e=zt(r=>$t(t.substance,r),n);return e.isErr()?st(e.error):await It(t.variation,t.style,...e.value)},"compileTrio"),me=c(async t=>{const n=Gt(),e=await Ot(t.shapes,n);if(e.isErr())throw Error(at(e.error));return ct({...t,labelCache:e.value})},"prepareState"),X=c(t=>t.params.optStatus==="EPConverged","stateConverged"),H=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function rt(t){const n=await fetch(t);if(!n.ok){console.error(`could not fetch ${t}`);return}return await n.text()}c(rt,"fetchResolver");class W extends Q.Component{canvasRef=Q.createRef();penroseState=void 0;timerID=void 0;constructor(n){super(n),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const n=await fe(this.props);n.isOk()?(this.penroseState=await me(n.value),this.setState({error:void 0})):this.setState({error:n.error})};converge=async()=>{if(this.penroseState){const n=he(this.penroseState);n.isOk()?this.penroseState=n.value:this.setState({error:n.error})}};tick=()=>{this.props.animate&&this.penroseState&&!X(this.penroseState)&&(this.penroseState=ft(this.penroseState,this.props.stepSize??1),this.renderCanvas())};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async n=>{if(this.props.domain!==n.domain||this.props.substance!==n.substance||this.props.style!==n.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==n.variation||this.props.animate!==n.animate){this.penroseState.variation=this.props.variation,this.penroseState=pe(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==n.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return L("div",{children:"rendering..."});{const n=this.canvasRef.current;if(this.penroseState){const e=await(this.props.interactive===!1?ce(this.penroseState,this.props.imageResolver??rt,this.props.name??""):ae(this.penroseState,async o=>{this.penroseState=o,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??rt,this.props.name??""));n.firstChild!==null?n.replaceChild(e,n.firstChild):n.appendChild(e),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return L("div",{children:"rendering..."})}};render=()=>{const{error:n}=this.state;return tt("div",{style:{width:"100%",height:"100%"},children:[!n&&L("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),n&&tt("div",{style:{padding:"1em",height:"100%"},children:[L("div",{style:{fontWeight:700},children:"1 error:"}),L("div",{style:{fontFamily:"monospace"},children:at(n).toString().split(`
`).map((e,o)=>L("p",{style:{margin:0},children:e},`err-ln-${o}`))})]})]})}}c(W,"Simple");try{W.displayName="Simple",W.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{W as S};
//# sourceMappingURL=Simple-31268321.js.map
