const n=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 27.2.0, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 1620.2 738.1" style="enable-background:new 0 0 1620.2 738.1;" xml:space="preserve">
<style type="text/css">
	.st0{fill:#FFFFFF;}
	.st1{fill:#8D374A;fill-opacity:0.5;}
	.st2{stroke:#000000;stroke-width:2;}
	.st3{fill:#FFFFFF;stroke:#000000;stroke-width:2;}
	.st4{stroke:#A0A389;stroke-width:5;stroke-opacity:0.5;}
	.st5{fill:#3CCA6D;fill-opacity:0.5;}
	.st6{fill:#AF578D;fill-opacity:0.5;}
	.st7{fill:#E2CAC5;fill-opacity:0.5;}
	.st8{fill:none;stroke:#787186;stroke-width:5;stroke-linejoin:round;}
	.st9{fill:#1A332A;fill-opacity:0.5;}
	.st10{fill:#63AE58;fill-opacity:0.5;}
	.st11{fill:#B2C252;fill-opacity:0.5;}
	.st12{fill:none;stroke:#DDDDDD;stroke-width:2;stroke-dasharray:10,8;}
</style>
<rect class="st0" width="1620.2" height="738.1"/>
<g>
	<polygon class="st1" points="196.7,314.1 741.7,230.7 347.9,69.5 	"/>
	<g>
		<path class="st2" d="M350.3,436.2l-21.8-142.3"/>
	</g>
	<circle class="st3" cx="328.5" cy="293.9" r="5"/>
	<g>
		<path class="st4" d="M597.6,667.2L773.2,368"/>
	</g>
	<ellipse class="st5" cx="79.5" cy="300.6" rx="56.3" ry="104.1"/>
	<rect x="507.4" y="314.6" class="st6" width="303.3" height="52.7"/>
	<circle class="st7" cx="177.2" cy="594.5" r="61.5"/>
	<g>
		<path class="st2" d="M350.3,436.2L222.6,553"/>
	</g>
	<circle class="st3" cx="222.6" cy="553" r="5"/>
	<g>
		<path class="st2" d="M350.3,436.2L635,603.4"/>
	</g>
	<circle class="st3" cx="635" cy="603.4" r="5"/>
	<g>
		<path class="st2" d="M350.3,436.2l-223.6-78.8"/>
	</g>
	<circle class="st3" cx="126.7" cy="357.4" r="5"/>
	<polyline class="st8" points="577.2,626.7 604.3,551.5 589.5,472.9 548.6,426.1 472.5,401.6 	"/>
	<g>
		<path class="st2" d="M350.3,436.2l122.2-34.7"/>
	</g>
	<circle class="st3" cx="472.5" cy="401.6" r="5"/>
	<g>
		<path class="st2" d="M350.3,436.2l157.1-69"/>
	</g>
	<circle class="st3" cx="507.4" cy="367.2" r="5"/>
	<circle cx="350.3" cy="436.2" r="5"/>
</g>
<penrose>
	<version>0.1</version>
	<variation>TingleRabbit0093</variation>
	<sub>Point x
Line L
Polyline M
Polygon P
Rectangle R
Circle C
Ellipse E</sub>
	<sty>canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Point x {
   vec2 x.pos = (?,?)
   shape x.icon = Circle {
      center: x.pos
      r: 5
      strokeWidth: 1
      fillColor: #000000ff
   } 
}

forall Line L {
   shape L.icon = Line {
      start: (?,?)
      end: (?,?)
      strokeWidth: 5
   }
}

forall Rectangle R {
   shape R.icon = Rectangle {
      center: (?, ?)
      width: ?
      height: ?
      strokeWidth: 1
   }

   ensure R.icon.width &gt; 50
   ensure R.icon.height &gt; 50
}

forall Polyline P {
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   vec2 p3 = (?,?)
   vec2 p4 = (?,?)
   
   shape P.icon = Polyline {
      points: [ p0, p1, p2, p3, p4 ]
      strokeWidth: 5.0
      strokeLinejoin: &quot;round&quot;
      strokeColor: sampleColor(1.0,&quot;rgb&quot;)
   }

   scalar L0 = norm(p1-p0)
   scalar L1 = norm(p2-p1)
   scalar L2 = norm(p3-p2)
   scalar L3 = norm(p4-p3)
   ensure L0 &gt; 30
   ensure L1 &gt; 30
   ensure L2 &gt; 30
   ensure L3 &gt; 30
   ensure L0 &lt; 80
   ensure L1 &lt; 80
   ensure L2 &lt; 80
   ensure L3 &lt; 80

   scalar theta0 = angleBetween(p1-p0,p2-p1)
   scalar theta1 = angleBetween(p2-p1,p3-p2)
   scalar theta2 = angleBetween(p3-p2,p4-p3)
   ensure theta0 &gt; MathPI()/6
   ensure theta1 &gt; MathPI()/6
   ensure theta2 &gt; MathPI()/6
}

forall Polygon P {
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   
   shape P.icon = Polygon {
      points: [ p0, p1, p2 ]
   }

   vec2 e01 = p1-p0
   vec2 e12 = p2-p1
   vec2 e20 = p0-p2

   ensure angleBetween( e01, -e12 ) &gt; toRadians(30)
   ensure angleBetween( e12, -e20 ) &gt; toRadians(30)
   ensure angleBetween( e20, -e01 ) &gt; toRadians(30)

   ensure abs(cross2D( e01, -e12 )) &gt; 50
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: ?
   }

   ensure C.icon.r &gt; 50
}

forall Ellipse E {
   shape E.icon = Ellipse {
      rx: ?
      ry: ?
   }

   ensure E.icon.rx &gt; 50
   ensure E.icon.ry &gt; 50
}

forall Group G {

   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)

   shape s1 = Circle {
      r: random(30,70)
   }
   shape s2 = Rectangle {
      width: random(50,150)
      height: random(50,150)
   }
   shape s3 = Polygon {
      points: [p0,p1,p2]
   }

   vec2 u = p1-p0
   vec2 v = p2-p1
   vec2 w = p0-p2
   ensure angleBetween(-u,v) &gt; MathPI()/6
   ensure angleBetween(-v,w) &gt; MathPI()/6
   ensure angleBetween(-w,u) &gt; MathPI()/6

   shape G.icon = Group {
      shapes: [s1,s2,s3]
   }

   shape bbox = Rectangle {
      fillColor: none()
      strokeColor: #ddd
      strokeDasharray: &quot;10 8&quot;
      strokeWidth: 2.0
      center: (0,0)
      width: random(300,500)
      height: random(300,500)
   }
   ensure contains( bbox, s1 )
   ensure contains( bbox, s2 )
   ensure contains( bbox, s3 )
   ensure contains( G.icon, bbox )

}

-- Rules for drawing closest points

forall Point x; Shape S
{
   vec2 p = closestPoint( S.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }
}

forall Point x; Polyline L
{
   vec2 p = closestPoint( L.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }
}

-- Keep shapes from overlapping
forall Shape S1; Shape S2
{
   ensure disjoint( S1.icon, S2.icon )
}
		</sty>
	<dsl>type Point

type Shape
type Line &lt;: Shape
type Polyline
type Polygon &lt;: Shape
type Rectangle &lt;: Shape
type Circle &lt;: Shape
type Ellipse &lt;: Shape

type Group &lt;: Shape
		</dsl>
</penrose>
<g>
	<g>
		<polygon class="st9" points="1437.2,325.5 979.3,341.4 1067.2,563.6 		"/>
		<circle class="st10" cx="1174.8" cy="210.4" r="41.5"/>
		<rect x="1304.4" y="491.5" class="st11" width="129.3" height="71.8"/>
	</g>
	<g>
		<path class="st2" d="M1122.4,622.2l-42.8-66.5"/>
	</g>
	<circle class="st3" cx="1079.5" cy="555.7" r="5"/>
	<g>
		<path class="st2" d="M1182.3,606.9l-53.4-83"/>
	</g>
	<circle class="st3" cx="1128.9" cy="523.9" r="5"/>
	<g>
		<path class="st2" d="M1102.5,149.9l40.5,33.9"/>
	</g>
	<circle class="st3" cx="1143" cy="183.8" r="5"/>
	<g>
		<path class="st2" d="M1301.1,186.1l-85.5,16.5"/>
	</g>
	<circle class="st3" cx="1215.6" cy="202.6" r="5"/>
	<g>
		<path class="st2" d="M913.3,394.9l75.4-29.8"/>
	</g>
	<circle class="st3" cx="988.7" cy="365" r="5"/>
	<g>
		<path class="st2" d="M931.7,21.5L1142,184.9"/>
	</g>
	<circle class="st3" cx="1142" cy="184.9" r="5"/>
	<g>
		<path class="st2" d="M1044.7,222.7l88.7-8.3"/>
	</g>
	<circle class="st3" cx="1133.5" cy="214.3" r="5"/>
	<g>
		<path class="st2" d="M1328.1,21.5L1201,178.2"/>
	</g>
	<circle class="st3" cx="1201" cy="178.2" r="5"/>
	<g>
		<path class="st2" d="M1132.1,553.7l-14.5-22.5"/>
	</g>
	<circle class="st3" cx="1117.7" cy="531.2" r="5"/>
	<g>
		<path class="st2" d="M1162.5,556l-24.4-38"/>
	</g>
	<circle class="st3" cx="1138.1" cy="518" r="5"/>
	<g>
		<path class="st2" d="M1048.7,710.5l18.4-146.8"/>
	</g>
	<circle class="st3" cx="1067.2" cy="563.6" r="5"/>
	<g>
		<path class="st2" d="M1015,488.8l19.5-7.7"/>
	</g>
	<circle class="st3" cx="1034.5" cy="481.1" r="5"/>
	<g>
		<path class="st2" d="M1316.5,431.4l-12.8-19.9"/>
	</g>
	<circle class="st3" cx="1303.7" cy="411.4" r="5"/>
	<g>
		<path class="st2" d="M1329.7,446.5l-23.6-36.6"/>
	</g>
	<circle class="st3" cx="1306.1" cy="409.9" r="5"/>
	<g>
		<path class="st2" d="M1435.3,701.2l-1.6-138"/>
	</g>
	<circle class="st3" cx="1433.7" cy="563.3" r="5"/>
	<g>
		<path class="st2" d="M1042.6,267.5l2.5,71.6"/>
	</g>
	<circle class="st3" cx="1045.1" cy="339.1" r="5"/>
	<g>
		<path class="st2" d="M878.7,149.5l100.6,192"/>
	</g>
	<circle class="st3" cx="979.3" cy="341.4" r="5"/>
	<g>
		<path class="st2" d="M1031.1,311.8l1,27.8"/>
	</g>
	<circle class="st3" cx="1032.1" cy="339.6" r="5"/>
	<g>
		<path class="st2" d="M1446.2,329.6l-9-4.1"/>
	</g>
	<circle class="st3" cx="1437.2" cy="325.5" r="5"/>
	<g>
		<path class="st2" d="M906.6,470.3l107-42.3"/>
	</g>
	<circle class="st3" cx="1013.6" cy="428" r="5"/>
	<g>
		<path class="st2" d="M1454.9,700.6l-21.2-137.3"/>
	</g>
	<circle class="st3" cx="1433.7" cy="563.3" r="5"/>
	<g>
		<path class="st2" d="M1092.7,431.8l-67.2,26.6"/>
	</g>
	<circle class="st3" cx="1025.6" cy="458.4" r="5"/>
	<g>
		<path class="st2" d="M1261.3,493.3h43"/>
	</g>
	<circle class="st3" cx="1304.4" cy="493.3" r="5"/>
	<g>
		<path class="st2" d="M1555.6,602.3l-122-39"/>
	</g>
	<circle class="st3" cx="1433.7" cy="563.3" r="5"/>
	<rect x="980.3" y="169.3" class="st12" width="455.9" height="393.3"/>
	<g>
		<path class="st2" d="M1328.4,391.6l1.8,2.7"/>
	</g>
	<circle class="st3" cx="1330.2" cy="394.4" r="5"/>
	<g>
		<path class="st2" d="M1032.8,522.8l15.8-6.2"/>
	</g>
	<circle class="st3" cx="1048.6" cy="516.6" r="5"/>
	<g>
		<path class="st2" d="M889.7,56.8l248.6,133.9"/>
	</g>
	<circle class="st3" cx="1138.3" cy="190.7" r="5"/>
	<g>
		<path class="st2" d="M846.4,372.4l132.9-30.9"/>
	</g>
	<circle class="st3" cx="979.3" cy="341.4" r="5"/>
	<g>
		<path class="st2" d="M912.8,28.6l227.9,158.1"/>
	</g>
	<circle class="st3" cx="1140.7" cy="186.7" r="5"/>
	<g>
		<path class="st2" d="M1600.5,26.7l-163.3,298.8"/>
	</g>
	<circle class="st3" cx="1437.2" cy="325.5" r="5"/>
	<g>
		<path class="st2" d="M928.7,689.1l138.5-125.5"/>
	</g>
	<circle class="st3" cx="1067.2" cy="563.6" r="5"/>
	<circle cx="889.7" cy="56.8" r="5"/>
	<circle cx="846.4" cy="372.4" r="5"/>
	<circle cx="1454.9" cy="700.6" r="5"/>
	<circle cx="1555.6" cy="602.3" r="5"/>
	<circle cx="1032.8" cy="522.8" r="5"/>
	<circle cx="1328.4" cy="391.6" r="5"/>
	<ellipse transform="matrix(0.1602 -0.9871 0.9871 0.1602 679.8371 1661.7968)" cx="1316.5" cy="431.4" rx="5" ry="5"/>
	<circle cx="1044.7" cy="222.7" r="5"/>
	<circle cx="1261.3" cy="493.3" r="5"/>
	<circle cx="1162.5" cy="556" r="5"/>
	<circle cx="912.8" cy="28.6" r="5"/>
	<circle cx="1600.5" cy="26.7" r="5"/>
	<circle cx="1182.3" cy="606.9" r="5"/>
	<circle cx="1329.7" cy="446.5" r="5"/>
	<circle cx="1446.2" cy="329.6" r="5"/>
	<circle cx="928.7" cy="689.1" r="5"/>
	<circle cx="1301.1" cy="186.1" r="5"/>
	<circle cx="931.7" cy="21.5" r="5"/>
	<circle cx="1132.1" cy="553.7" r="5"/>
	<circle cx="1102.5" cy="149.9" r="5"/>
	<circle cx="1015" cy="488.8" r="5"/>
	<circle cx="1092.7" cy="431.8" r="5"/>
	<circle cx="906.6" cy="470.3" r="5"/>
	<circle cx="1435.3" cy="701.2" r="5"/>
	<circle cx="1031.1" cy="311.8" r="5"/>
	<circle cx="1048.7" cy="710.5" r="5"/>
	<circle cx="878.7" cy="149.5" r="5"/>
	<circle cx="1328.1" cy="21.5" r="5"/>
	<circle cx="1122.4" cy="622.2" r="5"/>
	<circle cx="1042.6" cy="267.5" r="5"/>
	<circle cx="913.3" cy="394.9" r="5"/>
	<penrose>
		<version>0.1</version>
		<variation>DisarmRat557</variation>
		<sub>Point x0
Point x1
Point x2
Point x3
Point x4
Point x5
Point x6
Point x7
Point x8
Point x9
Point x10
Point x11
Point x12
Point x13
Point x14
Point x15
Point x16
Point x17
Point x18
Point x19
Point x20
Point x21
Point x22
Point x23
Point x24
Point x25
Point x26
Point x27
Point x28
Point x29
Point x30
Group G
			</sub>
		<sty>canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Point x {
   vec2 x.pos = (?,?)
   shape x.icon = Circle {
      center: x.pos
      r: 5
      strokeWidth: 1
      fillColor: #000000ff
   } 
}

forall Line L {
   shape L.icon = Line {
      start: (?,?)
      end: (?,?)
      strokeWidth: 5
   }
}

forall Rectangle R {
   shape R.icon = Rectangle {
      center: (?, ?)
      width: ?
      height: ?
      strokeWidth: 1
   }

   ensure R.icon.width &gt; 50
   ensure R.icon.height &gt; 50
}

forall Polyline P {
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   vec2 p3 = (?,?)
   vec2 p4 = (?,?)
   
   shape P.icon = Polyline {
      points: [ p0, p1, p2, p3, p4 ]
      strokeWidth: 5.0
      strokeLinejoin: &quot;round&quot;
      strokeColor: sampleColor(1.0,&quot;rgb&quot;)
   }

   scalar L0 = norm(p1-p0)
   scalar L1 = norm(p2-p1)
   scalar L2 = norm(p3-p2)
   scalar L3 = norm(p4-p3)
   ensure L0 &gt; 30
   ensure L1 &gt; 30
   ensure L2 &gt; 30
   ensure L3 &gt; 30
   ensure L0 &lt; 80
   ensure L1 &lt; 80
   ensure L2 &lt; 80
   ensure L3 &lt; 80

   scalar theta0 = angleBetween(p1-p0,p2-p1)
   scalar theta1 = angleBetween(p2-p1,p3-p2)
   scalar theta2 = angleBetween(p3-p2,p4-p3)
   ensure theta0 &gt; MathPI()/6
   ensure theta1 &gt; MathPI()/6
   ensure theta2 &gt; MathPI()/6
}

forall Polygon P {
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   
   shape P.icon = Polygon {
      points: [ p0, p1, p2 ]
   }

   vec2 e01 = p1-p0
   vec2 e12 = p2-p1
   vec2 e20 = p0-p2

   ensure angleBetween( e01, -e12 ) &gt; toRadians(30)
   ensure angleBetween( e12, -e20 ) &gt; toRadians(30)
   ensure angleBetween( e20, -e01 ) &gt; toRadians(30)

   ensure abs(cross2D( e01, -e12 )) &gt; 50
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: ?
   }

   ensure C.icon.r &gt; 50
}

forall Ellipse E {
   shape E.icon = Ellipse {
      rx: ?
      ry: ?
   }

   ensure E.icon.rx &gt; 50
   ensure E.icon.ry &gt; 50
}

forall Group G {

   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)

   shape s1 = Circle {
      r: random(30,70)
   }
   shape s2 = Rectangle {
      width: random(50,150)
      height: random(50,150)
   }
   shape s3 = Polygon {
      points: [p0,p1,p2]
   }

   vec2 u = p1-p0
   vec2 v = p2-p1
   vec2 w = p0-p2
   ensure angleBetween(-u,v) &gt; MathPI()/6
   ensure angleBetween(-v,w) &gt; MathPI()/6
   ensure angleBetween(-w,u) &gt; MathPI()/6

   shape G.icon = Group {
      shapes: [s1,s2,s3]
   }

   shape bbox = Rectangle {
      fillColor: none()
      strokeColor: #ddd
      strokeDasharray: &quot;10 8&quot;
      strokeWidth: 2.0
      center: (0,0)
      width: random(300,500)
      height: random(300,500)
   }
   ensure contains( bbox, s1 )
   ensure contains( bbox, s2 )
   ensure contains( bbox, s3 )
   ensure contains( G.icon, bbox )

}

-- Rules for drawing closest points

forall Point x; Shape S
{
   vec2 p = closestPoint( S.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }
}

forall Point x; Polyline L
{
   vec2 p = closestPoint( L.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }
}

-- Keep shapes from overlapping
forall Shape S1; Shape S2
{
   ensure disjoint( S1.icon, S2.icon )
}
			</sty>
		<dsl>type Point

type Shape
type Line &lt;: Shape
type Polyline
type Polygon &lt;: Shape
type Rectangle &lt;: Shape
type Circle &lt;: Shape
type Ellipse &lt;: Shape

type Group &lt;: Shape
			</dsl>
	</penrose>
</g>
</svg>
`;export{n as default};
//# sourceMappingURL=closest-point-examples.svg-ba672853.js.map
