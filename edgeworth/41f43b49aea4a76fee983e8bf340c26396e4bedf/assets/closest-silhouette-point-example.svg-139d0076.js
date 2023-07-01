const n=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 27.2.0, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 800 700" style="enable-background:new 0 0 800 700;" xml:space="preserve">
<style type="text/css">
	.st0{fill:#FFFFFF;}
	.st1{fill:#A63AC4;fill-opacity:0.5;}
	.st2{stroke:#000000;stroke-width:2;}
	.st3{fill:none;stroke:#000000;stroke-width:2;stroke-opacity:0.2;stroke-dasharray:10,8;}
	.st4{fill:#FFFFFF;stroke:#000000;stroke-width:2;}
	.st5{fill:none;stroke:#8ADD40;stroke-width:5;stroke-opacity:0.5;}
	.st6{fill:#A87363;fill-opacity:0.5;}
	.st7{fill:#991CB4;fill-opacity:0.5;}
	.st8{fill:#4C60A2;fill-opacity:0.5;}
	.st9{fill:none;stroke:#997098;stroke-width:5;stroke-linejoin:round;}
</style>
<rect class="st0" width="800" height="700"/>
<g>
	<polygon class="st1" points="190,450.3 142,509.4 280.8,700 342.5,612.7 	"/>
	<g>
		<path class="st2" d="M270,10L142,509.4"/>
	</g>
	<g>
		<path class="st3" d="M142,509.4l-263.9,1029.7"/>
	</g>
	<circle class="st4" cx="142" cy="509.4" r="5"/>
	<g>
		<path class="st5" d="M430.9,471.7l-81.9,43.9"/>
	</g>
	<ellipse class="st6" cx="270" cy="350" rx="54.2" ry="32.5"/>
	<rect x="505.3" y="358" class="st7" width="131.6" height="92.1"/>
	<circle class="st8" cx="206.4" cy="403" r="39"/>
	<g>
		<path class="st2" d="M270,10l-24.7,395.5"/>
	</g>
	<g>
		<path class="st3" d="M245.3,405.5l-66.2,1060.9"/>
	</g>
	<circle class="st4" cx="245.3" cy="405.5" r="5"/>
	<g>
		<path class="st2" d="M270,10l160.9,461.7"/>
	</g>
	<g>
		<path class="st3" d="M430.9,471.7l349.8,1003.8"/>
	</g>
	<circle class="st4" cx="430.9" cy="471.7" r="5"/>
	<g>
		<path class="st2" d="M270,10l54,336.9"/>
	</g>
	<g>
		<path class="st3" d="M324,346.9l168.2,1049.6"/>
	</g>
	<circle class="st4" cx="324" cy="346.9" r="5"/>
	<polyline class="st9" points="500.5,355.8 472.5,397.2 426.9,417.7 385,390.3 353.7,429.3 	"/>
	<g>
		<path class="st2" d="M270,10l230.5,345.8"/>
	</g>
	<g>
		<path class="st3" d="M500.5,355.8l589.7,884.5"/>
	</g>
	<circle class="st4" cx="500.5" cy="355.8" r="5"/>
	<g>
		<path class="st2" d="M270,10l235.3,440"/>
	</g>
	<g>
		<path class="st3" d="M505.3,450l501.3,937.4"/>
	</g>
	<circle class="st4" cx="505.3" cy="450" r="5"/>
	<circle cx="270" cy="10" r="5"/>
</g>
<penrose>
	<version>0.1</version>
	<variation>CherokeeDragonfly0873</variation>
	<sub>-- variation: TopsailBoar92821

Point x1
Line L
Polyline M
Polygon P
Rectangle R
Circle C
Ellipse E
-- Group G</sub>
	<sty>canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Point x {
   vec2 x.pos = (0,canvas.height/2 - 10)
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

forall Polyline M {
   vec2 p0 = (?,?)
   vec2 p1 = p0 + 50*circleRandom()
   vec2 p2 = p1 + 50*rotateBy(unit(p1-p0),random(0,1.5))
   vec2 p3 = p2 + 50*rotateBy(unit(p2-p1),random(0,1.5))
   vec2 p4 = p3 + 50*rotateBy(unit(p3-p2),random(-1.5,0))
   
   shape M.icon = Polyline {
      points: [ p0, p1, p2, p3, p4 ]
      strokeWidth: 5.0
      strokeLinejoin: &quot;round&quot;
      strokeColor: sampleColor(1.0, &quot;rgb&quot;)
   }
}

forall Polygon P {
   vec2 c = (?,?)
   scalar d = random(100,200)
   scalar u = circleRandom()
   scalar v = rot90(u)
   vec2 p0 = c + d*u
   vec2 p2 = c - d*u
   vec2 p1 = c + random(-150,150)*u + random(60,150)*v
   vec2 p3 = c + random(-150,150)*u - random(60,150)*v

   shape P.icon = Polygon {
      points: [ p0, p1, p2, p3 ]
   }
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: random(30,80)
   }
}

forall Ellipse E {
   shape E.icon = Ellipse {
      center: (0,0)
      rx: random(30,80)
      ry: random(30,80)
   }
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

-- Rules for drawing closest silhouette points

forall Point x; Shape S
{
   vec2 p = closestSilhouettePoint( S.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   scalar D = norm( (canvas.width, canvas.height) )
   shape continuedSegment = Line {
      start: p
      end: p + D*unit(p-x.pos)
      strokeColor: #00000033
      strokeWidth: 2
      ensureOnCanvas: false
      strokeDasharray: &quot;10 8&quot;
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
   vec2 p = closestSilhouettePoint( L.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   scalar D = norm( (canvas.width, canvas.height) )
   shape continuedSegment = Line {
      start: p
      end: p + D*unit(p-x.pos)
      strokeColor: #00000033
      strokeWidth: 2
      ensureOnCanvas: false
      strokeDasharray: &quot;10 8&quot;
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
</svg>
`;export{n as default};
