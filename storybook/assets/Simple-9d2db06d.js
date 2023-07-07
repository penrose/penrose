var gt=Object.defineProperty;var c=(t,e)=>gt(t,"name",{value:e,configurable:!0});import{i as at,t as q,a as Y,b as S,g as G,d as D,r as J,_ as yt,s as P,e as wt,m as Q,f as kt,h as At,j as tt,k as vt,n as St,o as bt,p as xt,q as Mt,u as Ct,v as Et,w as B,c as Rt,x as Nt,y as zt,A as U,B as Lt,C as $t,D as It,E as Tt,F as Vt}from"./svg-ee647ab8.js";import{t as jt,i as ct,m as Gt,c as Ot}from"./CollectLabels-452d0f40.js";import{R as et,j as z,a as nt}from"./jsx-runtime-671a6473.js";const rt={accentHeight:"accent-height",alignmentBaseline:"alignment-baseline",arabicForm:"arabic-form",baselineShift:"baseline-shift",capHeight:"cap-height",clipPath:"clip-path",clipRule:"clip-rule",colorInterpolation:"color-interpolation",colorInterpolationFilters:"color-interpolation-filters",colorProfile:"color-profile",colorRendering:"color-rendering",dominantBaseline:"dominant-baseline",enableBackground:"enable-background",fillOpacity:"fill-opacity",fillRule:"fill-rule",floodColor:"flood-color",floodOpacity:"flood-opacity",fontFamily:"font-family",fontSize:"font-size",fontSizeAdjust:"font-size-adjust",fontStretch:"font-stretch",fontStyle:"font-style",fontVariant:"font-variant",fontWeight:"font-weight",glyphName:"glyph-name",glyphOrientationHorizontal:"glyph-orientation-horizontal",glyphOrientationVertical:"glyph-orientation-vertical",horizAdvX:"horiz-adv-x",horizOriginX:"horiz-origin-x",imageRendering:"image-rendering",letterSpacing:"letter-spacing",lightingColor:"lighting-color",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",overlinePosition:"overline-position",overlineThickness:"overline-thickness",panose1:"panose-1",paintOrder:"paint-order",pointerEvents:"pointer-events",renderingIntent:"rendering-intent",shapeRendering:"shape-rendering",stopColor:"stop-color",stopOpacity:"stop-opacity",strikethroughPosition:"strikethrough-position",strikethroughThickness:"strikethrough-thickness",strokeDasharray:"stroke-dasharray",strokeDashoffset:"stroke-dashoffset",strokeLinecap:"stroke-linecap",strokeLinejoin:"stroke-linejoin",strokeMiterlimit:"stroke-miterlimit",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",textDecoration:"text-decoration",textRendering:"text-rendering",transformOrigin:"transform-origin",underlinePosition:"underline-position",underlineThickness:"underline-thickness",unicodeBidi:"unicode-bidi",unicodeRange:"unicode-range",unitsPerEm:"units-per-em",vAlphabetic:"v-alphabetic",vHanging:"v-hanging",vIdeographic:"v-ideographic",vMathematical:"v-mathematical",vectorEffect:"vector-effect",vertAdvY:"vert-adv-y",vertOriginX:"vert-origin-x",vertOriginY:"vert-origin-y",wordSpacing:"word-spacing",writingMode:"writing-mode"},x=c((t,e,r)=>{const n=["name","ensureOnCanvas"],o=new Set(r.concat(n));for(const[s,i]of t.passthrough)if(!(i.tag==="StrV"&&i.contents===""||o.has(s)))if(at(s,rt)){const a=rt[s];e.hasAttribute(a)||e.setAttribute(a,i.contents.toString())}else if(s==="style"&&i.contents!==""){const a=e.getAttribute(s);a===null?e.setAttribute(s,i.contents.toString()):e.setAttribute(s,`${a}${i.contents.toString()}`)}else e.hasAttribute(s)||e.setAttribute(s,i.contents.toString())},"attrAutoFillSvg"),C=c((t,e)=>{const r=t.fillColor,n=q(r.contents);return e.setAttribute("fill",Y(r.contents)),r.contents.tag!=="NONE"&&e.setAttribute("fill-opacity",n.toString()),["fillColor"]},"attrFill"),lt=c((t,e,r)=>{const n=t.center,[o,s]=S([n.contents[0],n.contents[1]],e);return r.setAttribute("cx",o.toString()),r.setAttribute("cy",s.toString()),["center"]},"attrCenter"),dt=c((t,e)=>{let r=t.scale.contents;r=r||1;let n=e.getAttribute("transform");return n=n===null?`scale(${r})`:n+`scale{${r}}`,e.setAttribute("transform",n),["scale"]},"attrScale"),pt=c((t,e,r)=>{const n=t.center,[o,s]=S([n.contents[0],n.contents[1]],e),i=t.width,a=t.height;let l=r.getAttribute("transform");return l=l===null?`translate(${o-i.contents/2}, ${s-a.contents/2})`:l+`translate(${o-i.contents/2}, ${s-a.contents/2})`,r.setAttribute("transform",l),["center","width","height"]},"attrTransformCoords"),Dt=c((t,e,r)=>{const n=t.center,[o,s]=S([n.contents[0],n.contents[1]],e),i=t.width,a=t.height;return r.setAttribute("x",(o-i.contents/2).toString()),r.setAttribute("y",(s-a.contents/2).toString()),["center","width","height"]},"attrXY"),_=c((t,e,r)=>{t.width,t.height;const n=t.center,o=t.rotation.contents,[s,i]=S([n.contents[0],n.contents[1]],e);let a=r.getAttribute("transform");return a=a===null?`rotate(${o}, ${s}, ${i})`:a+`rotate(${o}, ${s}, ${i})`,r.setAttribute("transform",a),["rotation","center","width","height"]},"attrRotation"),$=c((t,e)=>{const r=t.width,n=t.height;return e.setAttribute("width",r.contents.toString()),e.setAttribute("height",n.contents.toString()),["width","height"]},"attrWH"),Ft=c((t,e)=>{const r=t.cornerRadius;return e.setAttribute("rx",r.contents.toString()),["cornerRadius"]},"attrCornerRadius"),Xt=c((t,e)=>{const r=t.string,n=document.createTextNode(r.contents.toString());return e.appendChild(n),["string"]},"attrString"),qt="7,5",N=c((t,e)=>{const r=[],n=t.strokeColor,o=q(n.contents),s=t.strokeWidth.contents;return e.setAttribute("stroke",Y(n.contents)),r.push("strokeColor","strokeWidth"),n.contents.tag!=="NONE"&&(e.setAttribute("stroke-opacity",o.toString()),e.setAttribute("stroke-width",s.toString()),"strokeDasharray"in t&&t.strokeDasharray.contents!==""?(e.setAttribute("stroke-dasharray",t.strokeDasharray.contents),r.push("strokeDasharray")):"strokeStyle"in t&&t.strokeStyle.contents==="dashed"&&(e.setAttribute("stroke-dasharray",qt.toString()),r.push("strokeDasharray","strokeStyle")),"strokeLinecap"in t&&t.strokeLinecap.contents!==""&&(e.setAttribute("stroke-linecap",t.strokeLinecap.contents),r.push("strokeLinecap"))),r},"attrStroke"),M=c((t,e)=>{const r=t.name,n=document.createElementNS("http://www.w3.org/2000/svg","title");return n.textContent=r.contents,e.appendChild(n),["name"]},"attrTitle"),Yt=c((t,e)=>{const r=jt(t),n=e.getAttribute("style");return e.setAttribute("style",n?`${n}; font: ${r};`:`font: ${r};`),["fontFamily","fontSize","fontStretch","fontStyle","fontVariant","fontWeight","lineHeigh"]},"attrFont"),ut=c((t,e,r)=>{const o=t.points.contents.map(s=>S([s[0],s[1]],e));return r.setAttribute("points",o.toString()),["points"]},"attrPolyPoints"),Bt=c((t,{canvasSize:e})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","circle"),n=[];return n.push(...C(t,r)),n.push(...lt(t,e,r)),n.push(...N(t,r)),n.push(...M(t,r)),r.setAttribute("r",t.r.contents.toString()),n.push("r"),x(t,r,n),r},"RenderCircle"),_t=c((t,{canvasSize:e})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","ellipse"),n=[];return n.push(...C(t,r)),n.push(...lt(t,e,r)),n.push(...N(t,r)),n.push(...M(t,r)),r.setAttribute("rx",t.rx.contents.toString()),n.push("rx"),r.setAttribute("ry",t.ry.contents.toString()),n.push("ry"),x(t,r,n),r},"RenderEllipse"),W=c((t,[e,r],n)=>{const o=document.createElementNS("http://www.w3.org/2000/svg","text");return o.textContent=t,C(n,o),$(n,o),o.setAttribute("x",`${e}`),o.setAttribute("y",`${r}`),o.setAttribute("alignment-baseline","alphabetic"),o.setAttribute("dominant-baseline","alphabetic"),o.setAttribute("text-anchor","middle"),o},"placeholderString"),Wt=c((t,e)=>{const{canvasSize:r,labels:n,texLabels:o}=e,{center:s}=t,[i,a]=S([s.contents[0],s.contents[1]],r);if(o){const u=a+t.height.contents/2-t.descent.contents;let h=W(`$${G(t.string)}$`,[i,u],t);for(const[f,m]of t.passthrough)f==="texContourColor"&&m.contents!==""&&(h=W(`\\contour{${m.contents}}{$${G(t.string)}$}`,[i,u],t));return h}const l=document.createElementNS("http://www.w3.org/2000/svg","g"),p=[];p.push(..._(t,r,l)),p.push(...pt(t,r,l)),p.push(...M(t,l));const d=n.get(G(t.name));if(d&&d.tag==="EquationData"){const u=d.rendered.cloneNode(!0),h=u.getElementsByTagName("g")[0];p.push(...C(t,h)),p.push(...$(t,u)),h.setAttribute("stroke","none"),h.setAttribute("stroke-width","0");const f=t.fontSize;return u.setAttribute("style",`font-size: ${f.contents}`),l.appendChild(u),x(t,l,p),l}else return W(G(t.string),[i,a],t)},"RenderEquation"),Ut=`<?xml version='1.0' encoding='UTF-8' standalone='no'?>
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
</svg>`;let Ht=1;const Pt=c((t,e)=>{const r="--inject-",n={clipPath:["clip-path"],"color-profile":null,cursor:null,filter:null,linearGradient:["fill","stroke"],marker:["marker","marker-end","marker-mid","marker-start"],mask:null,pattern:["fill","stroke"],radialGradient:["fill","stroke"]},o=r+Ht++,s=/url\("?#([a-zA-Z][\w:.-]*)"?\)/g,i=t.querySelectorAll("[id]");let a;const l=e?new Set:void 0,p=new Set,d=[];let u=!1,h,f;if(i.length){for(h=0;h<i.length;h++){const y=i[h].localName;at(y,n)&&p.add(y)}p.forEach(y=>{(n[y]||[y]).forEach(function(g){d.indexOf(g)<0&&d.push(g)})}),d.length&&d.push("style");const m=t.getElementsByTagName("*");let w=t,v,k,A;for(h=-1;w!==null;){if(w.localName==="style")k=w.textContent,A=k&&k.replace(s,function(y,g){return l&&l.add(g),"url(#"+g+o+")"}),A!==k&&(w.textContent=A);else if(w.hasAttributes()){for(f=0;f<d.length;f++)v=d[f],k=w.getAttribute(v),A=k&&k.replace(s,function(y,g){return l&&l.add(g),"url(#"+g+o+")"}),A&&A!==k&&w.setAttribute(v,A);for(const y of["xlink:href","href"]){let g=w.getAttribute(y);g&&/^\s*#/.test(g)&&(g=g.trim(),w.setAttribute(y,g+o),l&&l.add(g.substring(1)))}}w=m.item(++h)}for(h=0;h<i.length;h++)a=i[h],(!l||l.has(a.id))&&(a.id+=o,u=!0)}return u},"makeIdsUnique"),Kt=c(async(t,{canvasSize:e,pathResolver:r})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","g"),o=[],s=t.href.contents;let i=await r(s);i===void 0&&(console.error(`Could not resolve image path ${s}`),i=Ut),o.push("href"),n.innerHTML=i;const a=n.querySelector("svg");return Pt(n,!1),o.push(...$(t,a)),o.push(..._(t,e,n)),o.push(...pt(t,e,n)),o.push(...M(t,n)),a.setAttribute("preserveAspectRatio",t.preserveAspectRatio.contents),o.push("preserveAspectRatio"),x(t,n,o),n},"RenderImage"),F=c((t,e,r,n,o,s)=>{const i=document.createElementNS("http://www.w3.org/2000/svg","marker");i.setAttribute("id",t),i.setAttribute("markerUnits","strokeWidth"),i.setAttribute("markerWidth",J(n.width*o).toString()),i.setAttribute("markerHeight",J(n.height*o).toString()),i.setAttribute("viewBox",n.viewbox),i.setAttribute("refX",n.refX.toString()),i.setAttribute("refY",n.refY.toString()),s?i.setAttribute("orient","auto"):i.setAttribute("orient","auto-start-reverse");const a=document.createElementNS("http://www.w3.org/2000/svg","path");return a.setAttribute("d",n.path),n.fillKind==="stroke"?(a.setAttribute("fill","none"),i.setAttribute("stroke",e),i.setAttribute("stroke-opacity",r.toString())):(a.setAttribute("fill",e),a.setAttribute("fill-opacity",r.toString())),n.style&&Object.entries(n.style).forEach(([l,p])=>{a.setAttribute(l,p)}),i.appendChild(a),i},"arrowHead"),Zt=c((t,e,r)=>{const n=[],[o,s]=[t.start.contents[0],t.start.contents[1]],[i,a]=[t.end.contents[0],t.end.contents[1]],l=t.startArrowheadSize.contents,p=t.endArrowheadSize.contents,d=t.strokeWidth.contents;n.push("start","end","startArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize","strokeWidth");const u=Math.sqrt((o-i)**2+(s-a)**2);if(u===0)return[[[o,s],[i,a]],n];let h,f;if(e){const k=(t.flipStartArrowhead.contents?e.refX:e.width-e.refX)*l*d,A=k/u*(o-i),y=k/u*(s-a);[h,f]=[o-A,s-y]}else[h,f]=[o,s];let m,w;if(r){const v=(r.width-r.refX)*p*d;[m,w]=[i-v/u*(i-o),a-v/u*(a-s)]}else[m,w]=[i,a];return[[[h,f],[m,w]],n]},"makeRoomForArrows"),Jt=c((t,{canvasSize:e,namespace:r,variation:n})=>{const o=D(t.startArrowhead.contents),s=D(t.endArrowhead.contents),[[[i,a],[l,p]],d]=Zt(t,o,s),u=Y(t.strokeColor.contents),h=q(t.strokeColor.contents),f=document.createElementNS("http://www.w3.org/2000/svg","g"),m=document.createElementNS("http://www.w3.org/2000/svg","line"),[w,v]=S([i,a],e),[k,A]=S([l,p],e);m.setAttribute("x1",w.toString()),m.setAttribute("y1",v.toString()),m.setAttribute("x2",k.toString()),m.setAttribute("y2",A.toString());const y=`${r}-${n}-${t.name.contents}`,g=y+"-startArrowId",E=y+"-endArrowId";if(o){const R=t.startArrowheadSize.contents,L=t.flipStartArrowhead.contents;f.appendChild(F(g,u,h,o,R,L))}if(s){const R=t.endArrowheadSize.contents;f.appendChild(F(E,u,h,s,R,!1))}return d.push("startArrowhead","flipStartArrowhead","endArrowhead","startArrowheadSize","endArrowheadSize"),d.push(...N(t,m)),o&&(m.setAttribute("marker-start",`url(#${g})`),d.push("startArrowhead")),s&&(m.setAttribute("marker-end",`url(#${E})`),d.push("endArrowhead")),f.appendChild(m),d.push(...M(t,f)),x(t,f,d),f},"RenderLine"),Qt=c((t,e)=>t.map(r=>{const{cmd:n,contents:o}=r;if(o.length===0&&n!=="Z")return console.error("WARNING: empty path"),"";const s=yt.flatten(o.map(i=>{switch(i.tag){case"CoordV":return S([i.contents[0],i.contents[1]],e);case"ValueV":return i.contents}})).join(" ");return`${n} ${s}`}).join(" "),"toPathString"),te=c(t=>{const e=document.createElementNS("http://www.w3.org/2000/svg","filter");return e.setAttribute("id",t),e.setAttribute("x","0"),e.setAttribute("y","0"),e.setAttribute("width","200%"),e.setAttribute("height","200%"),e.innerHTML=`
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
    `,e},"Shadow"),ee=c((t,{canvasSize:e})=>{const r=t.name.contents+"-startArrowId",n=t.name.contents+"-endArrowId",o=t.name.contents+"-shadow",s=document.createElementNS("http://www.w3.org/2000/svg","g"),i=Y(t.strokeColor.contents),a=q(t.strokeColor.contents),l=[],p=D(t.startArrowhead.contents),d=D(t.endArrowhead.contents);if(p){const h=t.name.contents+"-startArrowId",f=t.startArrowheadSize.contents,m=t.flipStartArrowhead.contents;s.appendChild(F(h,i,a,p,f,m))}if(d){const h=t.name.contents+"-endArrowId",f=t.endArrowheadSize.contents;s.appendChild(F(h,i,a,d,f,!1))}l.push("name","startArrowhead","flipStartArrowhead","endArrowhead"),s.appendChild(te(o));const u=document.createElementNS("http://www.w3.org/2000/svg","path");return l.push(...C(t,u)),l.push(...N(t,u)),u.setAttribute("d",Qt(t.d.contents,e)),l.push("d"),p&&(u.setAttribute("marker-start",`url(#${r})`),l.push("startArrowhead")),d&&(u.setAttribute("marker-end",`url(#${n})`),l.push("endArrowhead")),s.appendChild(u),l.push(...M(t,s)),x(t,s,l),s},"RenderPath"),ne=c((t,{canvasSize:e})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","polygon"),n=[];return n.push(...C(t,r)),n.push(...N(t,r)),n.push(...M(t,r)),n.push(...dt(t,r)),n.push(...ut(t,e,r)),x(t,r,n),r},"RenderPolygon"),re=c((t,{canvasSize:e})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","polyline"),n=[];return n.push(...C(t,r)),n.push(...N(t,r)),n.push(...M(t,r)),n.push(...dt(t,r)),n.push(...ut(t,e,r)),x(t,r,n),r},"RenderPolyline"),oe=c((t,{canvasSize:e})=>{const r=document.createElementNS("http://www.w3.org/2000/svg","rect"),n=[];return n.push(...Dt(t,e,r)),n.push(...$(t,r)),n.push(...C(t,r)),n.push(...N(t,r)),n.push(...M(t,r)),n.push(...Ft(t,r)),n.push(..._(t,e,r)),x(t,r,n),r},"RenderRectangle"),ie=c((t,{canvasSize:e,labels:r})=>{const n=document.createElementNS("http://www.w3.org/2000/svg","text"),o=[];o.push("x","y"),o.push(...C(t,n)),o.push(...N(t,n)),o.push(...M(t,n)),o.push(...Xt(t,n)),o.push(..._(t,e,n)),o.push(...Yt(t,n));const s=t.name,i=r.get(s.contents),a=t.center,[l,p]=S([a.contents[0],a.contents[1]],e);if(i&&i.tag==="TextData"){const d=i.descent.contents,u=i.height.contents,h=p+(u/2-d);n.setAttribute("x",l.toString()),n.setAttribute("y",h.toString()),o.push(...$(t,n))}else n.setAttribute("x",l.toString()),n.setAttribute("y",p.toString());return n.setAttribute("font-size-adjust",t.fontSizeAdjust.contents),n.setAttribute("alignment-baseline",t.alignmentBaseline.contents),n.setAttribute("dominant-baseline",t.dominantBaseline.contents),n.setAttribute("ascent",t.ascent.contents.toString()),n.setAttribute("descent",t.descent.contents.toString()),n.setAttribute("text-anchor",t.textAnchor.contents.toString()),n.setAttribute("visibility",t.visibility.contents),o.push("fontSizeAdjust","alignmentBaseline","dominantBaseline","ascent","descent","textAnchor","visibility"),x(t,n,o),n},"RenderText"),se=c((t,e,r,n)=>{const o=[...t.varyingValues];return{...t,params:P(o.length),varyingValues:o}},"dragUpdate"),ot=c(({clientX:t,clientY:e},r)=>{const n=r.getScreenCTM();return n!==null?{x:(t-n.e)/n.a,y:(e-n.f)/n.d}:{x:0,y:0}},"getPosition"),ae=c(async(t,e,r,n)=>{const o=document.createElementNS("http://www.w3.org/2000/svg","svg");o.setAttribute("xmlns","http://www.w3.org/2000/svg"),o.setAttribute("version","1.2"),o.setAttribute("viewBox",`0 0 ${t.canvas.width} ${t.canvas.height}`);const s=c((a,l,p)=>{e(se(t))},"onDrag"),i=t.computeShapes(t.varyingValues);return await ht(i,o,{labels:t.labelCache,canvasSize:t.canvas.size,variation:t.variation,namespace:n,texLabels:!1,pathResolver:r},{updateState:e,onDrag:s,parentSVG:o}),o},"toInteractiveSVG"),ce=c(async(t,e,r,n=!1)=>{const{varyingValues:o,computeShapes:s,labelCache:i,canvas:a,variation:l}=t,p=document.createElementNS("http://www.w3.org/2000/svg","svg");p.setAttribute("version","1.2"),p.setAttribute("xmlns","http://www.w3.org/2000/svg"),p.setAttribute("viewBox",`0 0 ${a.width} ${a.height}`);const d=s(o),u=d.map(b=>wt(b)),h=Q(u.map(b=>kt(b))),f=Q(u.map(b=>At(b))),m=tt(u.map(b=>vt(b))),w=tt(u.map(b=>St(b))),v=[h,f,m,w],[k,A,y,g]=(await bt(xt(v)))(b=>b.val).secondary,[E,R]=S([k,A],[a.width,a.height]),[L,I]=S([y,g],[a.width,a.height]),T=[E,I],V=[L-E,R-I];p.setAttribute("penrose","0");const j=document.createElementNS("https://penrose.cs.cmu.edu/metadata","penrose"),Z=document.createElementNS("https://penrose.cs.cmu.edu/croppedViewBox","croppedViewBox");return Z.insertAdjacentText("afterbegin",`${T[0]} ${T[1]} ${V[0]} ${V[1]}`),j.appendChild(Z),p.appendChild(j),await ht(d,p,{labels:i,canvasSize:a.size,variation:l,namespace:r,texLabels:n,pathResolver:e},void 0),p},"toSVG"),le=c(async(t,e,r)=>{const n=document.createElementNS("http://www.w3.org/2000/svg","g"),o=t.clipPath.contents;let s,i;if(o.tag==="Clip"){const l=o.contents;s=l.name.contents;const p=await O(l,e,r),d=document.createElementNS("http://www.w3.org/2000/svg","clipPath");i=e.namespace+s+"-clip",d.setAttribute("id",i),d.appendChild(p),n.appendChild(d)}const a=t.shapes.contents;for(const l of a){const p=l.name.contents;if(o.tag==="Clip"){if(p!==s){const d=await O(l,e,r),u=document.createElementNS("http://www.w3.org/2000/svg","g");u.appendChild(d),u.setAttribute("clip-path",`url(#${i})`),n.appendChild(u)}}else{const d=await O(l,e,r);n.appendChild(d)}}return x(t,n,[...M(t,n),"shapes","clipPath"]),n},"RenderGroup"),de=c(async(t,e)=>{switch(t.shapeType){case"Circle":return Bt(t,e);case"Ellipse":return _t(t,e);case"Equation":return Wt(t,e);case"Image":return Kt(t,e);case"Line":return Jt(t,e);case"Path":return ee(t,e);case"Polygon":return ne(t,e);case"Polyline":return re(t,e);case"Rectangle":return oe(t,e);case"Text":return ie(t,e)}},"RenderShapeSvg"),O=c(async(t,e,r)=>{if(t.shapeType==="Group")return await le(t,e,r);{const n=await de(t,e);if(r){const o=document.createElementNS("http://www.w3.org/2000/svg","g");Mt(t)?o.setAttribute("pointer-events","visibleStroke"):Ct(t)?o.setAttribute("pointer-events","bounding-box"):o.setAttribute("pointer-events","auto"),o.appendChild(n);const s=c(i=>{const{clientX:a,clientY:l}=i,{x:p,y:d}=ot({clientX:a,clientY:l},r.parentSVG),{width:u,height:h,x:f,y:m}=i.target.getBBox({stroke:!0}),w=p-f,v=e.canvasSize[0]-u+(p-f),k=d-m,A=e.canvasSize[1]-h+(d-m);o.setAttribute("opacity","0.5");let y=0,g=0;const E=c(L=>{const{x:I,y:T}=ot(L,r.parentSVG),V=it(I,w,v),j=it(T,k,A);y=V-p,g=d-j,o.setAttribute("transform",`translate(${y},${-g})`)},"onMouseMove"),R=c(()=>{o.setAttribute("opacity","1"),document.removeEventListener("mouseup",R),document.removeEventListener("mousemove",E),r.onDrag(t.name.contents,y,g)},"onMouseUp");document.addEventListener("mouseup",R),document.addEventListener("mousemove",E)},"onMouseDown");return o.addEventListener("mousedown",s),o}else return n}},"RenderShape"),ht=c(async(t,e,r,n)=>{for(const o of t){const s=await O(o,r,n);e.appendChild(s)}},"RenderShapes"),it=c((t,e,r)=>Math.min(Math.max(t,e),r),"clamp"),pe=c(t=>{const e=Et(t.variation);return ct({...t,varyingValues:t.inputs.map(({meta:r})=>r.init.tag==="Sampled"?r.init.sampler(e):r.init.pending),currentStageIndex:0,params:P(t.varyingValues.length)})},"resample"),ft=c((t,e)=>{const{constraintSets:r,optStages:n,currentStageIndex:o}=t,s=n[o],i=$t(r.get(s),"missing stage"),a=new Float64Array(t.varyingValues),l=It((p,d,u)=>t.gradient(i,p,d,u).phi,a,t.params,e.until);return l.optStatus==="Error"?U({errorType:"RuntimeError",...Tt("",t)}):B({...t,varyingValues:Array.from(a),params:l})},"step"),ue=c((t,e=1e4)=>{let r=0;const n=ft(t,{until:()=>r++>=e});if(n.isErr())return n;{const o=n.value;if(X(o)&&!K(o)){const s=mt(o);return B(s)}else return n}},"stepTimes"),mt=c(t=>K(t)?t:{...t,currentStageIndex:t.currentStageIndex+1,params:P(t.varyingValues.length)},"nextStage"),he=c(t=>{let e=t;for(;!X(e)||!K(e);){X(e)&&(e=mt(e));const r=ft(e,{until:()=>!1});if(r.isOk())e=r.value;else return r}return B(e)},"optimize"),fe=c(async t=>{const e=Rt(t.domain),r=Nt(o=>zt(t.substance,o),e),n=r.isErr()?U(r.error):await Lt(t.variation,t.style,t.excludeWarnings??[],...r.value);if(n.isErr())return n;{const o=n.value,s=Gt(),i=await Ot(o.shapes,s);return i.isErr()?U(i.error):B(ct({...o,labelCache:i.value}))}},"compile"),X=c(t=>t.params.optStatus==="EPConverged","isOptimized"),K=c(t=>t.currentStageIndex===t.optStages.length-1,"finalStage");async function st(t){const e=await fetch(t);if(!e.ok){console.error(`could not fetch ${t}`);return}return await e.text()}c(st,"fetchResolver");class H extends et.Component{canvasRef=et.createRef();penroseState=void 0;timerID=void 0;constructor(e){super(e),this.state={error:void 0}}compile=async()=>{this.penroseState=void 0,this.setState({error:void 0});const e=await fe(this.props);e.isOk()?(this.penroseState=e.value,this.setState({error:void 0})):this.setState({error:e.error})};converge=async()=>{if(this.penroseState){const e=he(this.penroseState);e.isOk()?this.penroseState=e.value:this.setState({error:e.error})}};tick=()=>{if(this.props.animate&&this.penroseState&&!X(this.penroseState)){const e=ue(this.penroseState,this.props.stepSize??1);e.isErr()?this.setState({error:e.error}):this.penroseState=e.value,this.renderCanvas()}};componentDidMount=async()=>{await this.compile(),this.props.animate||await this.converge(),this.renderCanvas(),this.timerID=window.setInterval(()=>this.tick(),1e3/60)};componentDidUpdate=async e=>{if(this.props.domain!==e.domain||this.props.substance!==e.substance||this.props.style!==e.style){await this.compile(),this.props.animate||await this.converge(),this.renderCanvas();return}if(this.penroseState&&!this.state.error){if(this.props.variation!==e.variation||this.props.animate!==e.animate){this.penroseState.variation=this.props.variation,this.penroseState=pe(this.penroseState),this.props.animate||await this.converge(),this.renderCanvas();return}else if(this.props.interactive!==e.interactive){this.renderCanvas();return}}};componentWillUnmount=()=>{clearInterval(this.timerID)};renderCanvas=async()=>{if(this.canvasRef.current===null)return z("div",{children:"rendering..."});{const e=this.canvasRef.current;if(this.penroseState){const r=await(this.props.interactive===!1?ce(this.penroseState,this.props.imageResolver??st,this.props.name??""):ae(this.penroseState,async n=>{this.penroseState=n,this.props.animate||await this.converge(),this.renderCanvas()},this.props.imageResolver??st,this.props.name??""));e.firstChild!==null?e.replaceChild(r,e.firstChild):e.appendChild(r),this.props.onFrame&&this.props.onFrame(this.penroseState)}else return z("div",{children:"rendering..."})}};render=()=>{const{error:e}=this.state;return nt("div",{style:{width:"100%",height:"100%"},children:[!e&&z("div",{style:{width:"100%",height:"100%"},ref:this.canvasRef}),e&&nt("div",{style:{padding:"1em",height:"100%"},children:[z("div",{style:{fontWeight:700},children:"1 error:"}),z("div",{style:{fontFamily:"monospace"},children:Vt(e).toString().split(`
`).map((r,n)=>z("p",{style:{margin:0},children:r},`err-ln-${n}`))})]})]})}}c(H,"Simple");try{H.displayName="Simple",H.__docgenInfo={description:"",displayName:"Simple",props:{domain:{defaultValue:null,description:"",name:"domain",required:!0,type:{name:"string"}},substance:{defaultValue:null,description:"",name:"substance",required:!0,type:{name:"string"}},style:{defaultValue:null,description:"",name:"style",required:!0,type:{name:"string"}},variation:{defaultValue:null,description:"",name:"variation",required:!0,type:{name:"string"}},excludeWarnings:{defaultValue:null,description:"",name:"excludeWarnings",required:!1,type:{name:"string[]"}},stepSize:{defaultValue:null,description:"",name:"stepSize",required:!1,type:{name:"number"}},interactive:{defaultValue:null,description:"",name:"interactive",required:!1,type:{name:"boolean"}},animate:{defaultValue:null,description:"",name:"animate",required:!1,type:{name:"boolean"}},onFrame:{defaultValue:null,description:"",name:"onFrame",required:!1,type:{name:"((frame: State) => void)"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!1,type:{name:"PathResolver"}},name:{defaultValue:null,description:"",name:"name",required:!1,type:{name:"string"}}}}}catch{}export{H as S,X as i};
//# sourceMappingURL=Simple-9d2db06d.js.map
