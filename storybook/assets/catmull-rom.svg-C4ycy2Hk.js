const t=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 27.2.0, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 425 400" style="enable-background:new 0 0 425 400;" xml:space="preserve">
<style type="text/css">
	.st0{fill:#0055CC;fill-opacity:0.1333;}
	.st1{fill:none;stroke:#1B8A1F;stroke-width:0.8333;stroke-linejoin:round;stroke-miterlimit:3.3333;stroke-opacity:0.2;}
	.st2{fill:none;stroke:#1B8A1F;stroke-width:1.1111;stroke-linejoin:round;stroke-miterlimit:3.3333;stroke-opacity:0.6;}
	.st3{fill:none;stroke:#1B8A1F;stroke-width:1.3889;stroke-linejoin:round;stroke-miterlimit:3.3333;stroke-opacity:0.6;}
	.st4{fill:none;stroke:#1B8A1F;stroke-width:1.6667;stroke-linejoin:round;stroke-miterlimit:3.3333;}
	.st5{fill:#FFFFFF;stroke:#000000;stroke-width:1.25;stroke-miterlimit:3.3333;}
	.st6{fill:#112277;fill-opacity:0.8;}
	.st7{fill:#FFDD00;}
	.st8{font-family:'Menlo-Regular';}
	.st9{font-size:7.5px;}
</style>
<g>
	<rect x="0" class="st0" width="425" height="200"/>
	<g>
		<path class="st1" d="M342.8,165.8l5.1-96.4L175.8,26l-27.1,15.4l133.5-30.9l16.6,30.2L80,99.3L218.1,131L342.8,165.8z"/>
	</g>
	<g>
		<path class="st2" d="M342.8,165.8c10.8-5.1,19-84.8,5.1-96.4S192.4,28.3,175.8,26s-36,16.7-27.1,15.4c8.9-1.3,121-30.8,133.5-30.9
			c12.5-0.1,33.5,22.8,16.6,30.2S86.7,91.8,80,99.3c-6.7,7.5,116.2,26.1,138.1,31.7C240,136.5,332,170.9,342.8,165.8z"/>
	</g>
	<g>
		<path class="st3" d="M342.8,165.8c21.6-10.3,32.9-73.1,5.1-96.4S209,30.7,175.8,26S131,44,148.7,41.4
			c17.7-2.6,108.5-30.8,133.5-30.9c25-0.1,50.3,15.4,16.6,30.2S93.5,84.3,80,99.3s94.3,20.6,138.1,31.7S321.2,176.1,342.8,165.8z"/>
	</g>
	<g>
		<path class="st4" d="M342.8,165.8c32.5-15.4,46.8-61.5,5.1-96.4S225.6,33,175.8,26s-53.7,19.3-27.1,15.4s96-30.7,133.5-30.9
			c37.5-0.2,67.2,8,16.6,30.2S100.2,76.7,80,99.3s72.4,15,138.1,31.7S310.4,181.2,342.8,165.8z"/>
	</g>
	<circle class="st5" cx="175.8" cy="26" r="2.5"/>
	<circle class="st5" cx="282.3" cy="10.5" r="2.5"/>
	<circle class="st5" cx="148.7" cy="41.4" r="2.5"/>
	<circle class="st5" cx="218.1" cy="131" r="2.5"/>
	<circle class="st5" cx="80" cy="99.3" r="2.5"/>
	<circle class="st5" cx="347.9" cy="69.4" r="2.5"/>
	<circle class="st5" cx="342.8" cy="165.8" r="2.5"/>
	<circle class="st5" cx="298.9" cy="40.7" r="2.5"/>
	<rect x="32.5" y="149.9" class="st6" width="300.7" height="43.9"/>
	<g>
		<text transform="matrix(1 0 0 1 40.8428 164.4043)" class="st7 st8 st9">shape spline = Path {</text>
		<text transform="matrix(1 0 0 1 54.4595 174.3428)" class="st7 st8 st9">d: interpolatingSpline( \u201Cclosed\u201D, [ p1, ..., pN ], tension )</text>
		<text transform="matrix(1 0 0 1 40.9966 184.8281)" class="st7 st8 st9">}</text>
	</g>
	<penrose>
		<croppedViewBox>0 0 510 240</croppedViewBox>
		<version>0.1</version>
		<variation>CelloSnail125</variation>
		<sub>Point p1, p2, p3, p4, p5, p6, p7, p8
InterpolatingCurve c</sub>
		<sty>canvas {
   width = 510
   height = 240
}

global {
   shape background = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: #05c2
   }

   shape line1 = Text {
      string: &quot;shape spline = Path {&quot;
      fontFamily: &quot;Menlo&quot;
      fontSize: &quot;9px&quot;
      fillColor: #ffdd00
   }
   shape line2 = Text {
      center: line1.center + (122,-12)
      string: &quot;d: interpolatingSpline( \u201Cclosed\u201D, [ p1, ..., pN ], tension )&quot;
      fontFamily: line1.fontFamily
      fontSize: line1.fontSize
      fillColor: line1.fillColor
   }
   shape line3 = Text {
      center: line1.center + (-54,-24)
      string: &quot;}&quot;
      fontFamily: line1.fontFamily
      fontSize: line1.fontSize
      fillColor: line1.fillColor
   }
   shape code = Group {
      shapes: [ line1, line2, line3 ]
   }

   shape textBox = Rectangle {
      fillColor: #127c
   }
   encourage textBox.width == 0
   encourage textBox.height == 0
   ensure contains( textBox, code, 10 )

   layer code above textBox
   layer textBox above background
}

forall Point p {

   vec2 p.x = (?,?)

   shape p.icon = Circle {
      center: p.x
      r: 3
      fillColor: #fff
      strokeColor: #000
      strokeWidth: 1.5
   }

   layer global.textBox above p.icon
}

collect Point p into points {

   centers = listof x from points

   scalar tension = .25
   scalar baseWidth = 1
   scalar width = 1

   string closing = &quot;closed&quot;

   shape spline0 = Path {
      d: interpolatingSpline( closing, centers, 0*tension/3 )
      strokeColor: #1b8a1f33
      strokeWidth: baseWidth + 0*width/3
      strokeLinejoin: &quot;round&quot;
   }
   shape spline1 = Path {
      d: interpolatingSpline( closing, centers, 1*tension/3 )
      strokeColor: #1b8a1f99
      strokeWidth: baseWidth + 1*width/3
      strokeLinejoin: &quot;round&quot;
   }
   shape spline2 = Path {
      d: interpolatingSpline( closing, centers, 2*tension/3 )
      strokeColor: #1b8a1f99
      strokeWidth: baseWidth + 2*width/3
      strokeLinejoin: &quot;round&quot;
   }
   shape global.spline3 = Path {
      d: interpolatingSpline( closing, centers, 3*tension/3 )
      strokeColor: #1b8a1fff
      strokeWidth: baseWidth + 3*width/3
      strokeLinejoin: &quot;round&quot;
   }

   layer global.textBox above global.spline3
   layer global.spline3 above spline2
   layer spline2 above spline1
   layer spline1 above spline0
   layer spline0 above global.background
}

forall Point p {
   layer p.icon above global.spline3
}
			</sty>
		<dsl>type Point
type InterpolatingCurve</dsl>
	</penrose>
</g>
<g>
	<rect y="200" class="st0" width="425" height="200"/>
	<g>
		<path class="st1" d="M3.9,205.2l116.7,37.5L174,366.9l157.3,5.2l49.3-113.7L176.3,379l146.3-136.3l-129.1-33.3"/>
	</g>
	<g>
		<path class="st2" d="M3.9,205.2c9.7,3.1,102.6,24.1,116.7,37.5c14.2,13.5,35.8,113.4,53.4,124.2s140.1,14.2,157.3,5.2
			c17.2-9,62.2-114.3,49.3-113.7C367.7,259,181.1,380.3,176.3,379c-4.8-1.3,144.9-122.1,146.3-136.3c1.4-14.1-118.4-30.5-129.1-33.3
			"/>
	</g>
	<g>
		<path class="st3" d="M3.9,205.2c19.5,6.3,88.4,10.6,116.7,37.5s18.3,102.6,53.4,124.2c35.1,21.6,122.9,23.3,157.3,5.2
			c34.4-18.1,75.1-114.8,49.3-113.7S185.9,381.6,176.3,379c-9.7-2.6,143.4-108,146.3-136.3c2.9-28.3-107.6-27.8-129.1-33.3"/>
	</g>
	<g>
		<path class="st4" d="M3.9,205.2c29.2,9.4,74.2-2.9,116.7,37.5s0.7,91.8,53.4,124.2s105.7,32.3,157.3,5.2s88.1-115.4,49.3-113.7
			S190.8,382.9,176.3,379c-14.5-3.9,142-93.9,146.3-136.3c4.3-42.4-96.9-25-129.1-33.3"/>
	</g>
	<circle class="st5" cx="174" cy="366.9" r="2.5"/>
	<circle class="st5" cx="380.6" cy="258.4" r="2.5"/>
	<circle class="st5" cx="331.3" cy="372.1" r="2.5"/>
	<circle class="st5" cx="193.4" cy="209.4" r="2.5"/>
	<circle class="st5" cx="322.6" cy="242.7" r="2.5"/>
	<circle class="st5" cx="120.6" cy="242.8" r="2.5"/>
	<circle class="st5" cx="3.9" cy="205.2" r="2.5"/>
	<circle class="st5" cx="176.3" cy="379" r="2.5"/>
	<rect x="11.1" y="284.2" class="st6" width="296.2" height="43.9"/>
	<g>
		<text transform="matrix(1 0 0 1 19.4858 298.6465)" class="st7 st8 st9">shape spline = Path {</text>
		<text transform="matrix(1 0 0 1 37.6182 308.584)" class="st7 st8 st9">d: interpolatingSpline( \u201Copen\u201D, [ p1, ..., pN ], tension )</text>
		<text transform="matrix(1 0 0 1 19.6396 319.0693)" class="st7 st8 st9">}</text>
	</g>
</g>
<penrose>
	<croppedViewBox>0 0 510 240</croppedViewBox>
	<version>0.1</version>
	<variation>ElevatedMoose47542</variation>
	<sub>Point p1, p2, p3, p4, p5, p6, p7, p8
InterpolatingCurve c</sub>
	<sty>canvas {
   width = 510
   height = 240
}

global {
   shape background = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: #05c2
   }

   shape line1 = Text {
      string: &quot;shape spline = Path {&quot;
      fontFamily: &quot;Menlo&quot;
      fontSize: &quot;9px&quot;
      fillColor: #ffdd00
   }
   shape line2 = Text {
      center: line1.center + (122,-12)
      string: &quot;d: interpolatingSpline( \u201Copen\u201D, [ p1, ..., pN ], tension )&quot;
      fontFamily: line1.fontFamily
      fontSize: line1.fontSize
      fillColor: line1.fillColor
   }
   shape line3 = Text {
      center: line1.center + (-54,-24)
      string: &quot;}&quot;
      fontFamily: line1.fontFamily
      fontSize: line1.fontSize
      fillColor: line1.fillColor
   }
   shape code = Group {
      shapes: [ line1, line2, line3 ]
   }

   shape textBox = Rectangle {
      fillColor: #127c
   }
   encourage textBox.width == 0
   encourage textBox.height == 0
   ensure contains( textBox, code, 10 )

   layer code above textBox
   layer textBox above background
}

forall Point p {

   vec2 p.x = (?,?)

   shape p.icon = Circle {
      center: p.x
      r: 3
      fillColor: #fff
      strokeColor: #000
      strokeWidth: 1.5
   }

   layer global.textBox above p.icon
}

collect Point p into points {

   centers = listof x from points

   scalar tension = .25
   scalar baseWidth = 1
   scalar width = 1

   shape spline0 = Path {
      d: interpolatingSpline( &quot;open&quot;, centers, 0*tension/3 )
      strokeColor: #1b8a1f33
      strokeWidth: baseWidth + 0*width/3
      strokeLinejoin: &quot;round&quot;
   }
   shape spline1 = Path {
      d: interpolatingSpline( &quot;open&quot;, centers, 1*tension/3 )
      strokeColor: #1b8a1f99
      strokeWidth: baseWidth + 1*width/3
      strokeLinejoin: &quot;round&quot;
   }
   shape spline2 = Path {
      d: interpolatingSpline( &quot;open&quot;, centers, 2*tension/3 )
      strokeColor: #1b8a1f99
      strokeWidth: baseWidth + 2*width/3
      strokeLinejoin: &quot;round&quot;
   }
   shape global.spline3 = Path {
      d: interpolatingSpline( &quot;open&quot;, centers, 3*tension/3 )
      strokeColor: #1b8a1fff
      strokeWidth: baseWidth + 3*width/3
      strokeLinejoin: &quot;round&quot;
   }

   layer global.textBox above global.spline3
   layer global.spline3 above spline2
   layer spline2 above spline1
   layer spline1 above spline0
   layer spline0 above global.background
}

forall Point p {
   layer p.icon above global.spline3
}
		</sty>
	<dsl>type Point
type InterpolatingCurve</dsl>
</penrose>
</svg>
`;export{t as default};
