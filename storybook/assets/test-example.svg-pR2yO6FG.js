const e=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 27.2.0, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 214.9963074 118.2432404" style="enable-background:new 0 0 214.9963074 118.2432404;" xml:space="preserve">
<style type="text/css">
	.st0{fill:#00FF00;}
	.st1{fill:none;stroke:#FF0000;stroke-width:4}
	.st2{fill:none;stroke:#00EE00;stroke-width:4.25}
	.st3{fill:#FFFFFF;}
	.st4{font-family:'HelveticaNeue-CondensedBold';}
	.st5{font-size:12px;}
	.st6{fill:none;stroke:#FFFFFF;stroke-width:2}
	.st7{fill:#231F20;}
</style>
<rect x="5.2702818" y="1.6891892" class="st0" width="100" height="100"/>
<g>
	<line class="st1" x1="40.8537827" y1="73.2089844" x2="70.1484756" y2="77.4155884"/>
	<line class="st1" x1="40.8537827" y1="73.2089844" x2="47.403141" y2="34.1493912"/>
	<line class="st1" x1="40.8537827" y1="73.2089844" x2="33.8427811" y2="65.0222855"/>
	<line class="st1" x1="70.1484756" y1="77.4155884" x2="47.403141" y2="34.1493912"/>
	<line class="st1" x1="47.403141" y1="34.1493912" x2="33.8427811" y2="65.0222855"/>
	<line class="st1" x1="33.8427811" y1="65.0222855" x2="70.1484756" y2="77.4155884"/>
</g>
<g>
	<line class="st2" x1="40.8538322" y1="73.2089539" x2="70.1484756" y2="77.4155884"/>
	<line class="st2" x1="40.8538322" y1="73.2089539" x2="47.403141" y2="34.1494217"/>
	<line class="st2" x1="40.8538322" y1="73.2089539" x2="33.8427734" y2="65.0223236"/>
	<line class="st2" x1="70.1484756" y1="77.4155884" x2="47.403141" y2="34.1494217"/>
	<line class="st2" x1="47.403141" y1="34.1494217" x2="33.8427734" y2="65.0223236"/>
	<line class="st2" x1="33.8427734" y1="65.0223236" x2="70.1484756" y2="77.4155884"/>
</g>
<penrose>
	<croppedViewBox>0 0 100 100</croppedViewBox>
	<version>0.1</version>
	<variation>FudgesicleWoodpecker2520</variation>
	<sub>Shape S
rotate3d(S)</sub>
	<sty>canvas {
   width = 100
   height = 100

   shape background = Rectangle {
      fillColor: #0f0
      center: (0,0)
      width: width
      height: height
   }
}

global {
   -- Due to rasterization effects (such as anti-aliasing),
   -- the reference and computed shapes may not match exactly.
   -- For instance, even two shapes with identical coordinates
   -- but different colors may yield a &quot;fringing&quot; around the
   -- border.  This parameter determines a &quot;fudge factor&quot; that
   -- is used to compare shapes, e.g., by drawing a small
   -- margin around the computed shape.
   scalar tolerance = 0.25
}

-- Use an asymmetric tetrahedron as the test shape, since
-- it does not have any Euclidean symmetries (i.e., it
-- cannot be mapped to itself by rotation, reflection,
-- translation, etc.), which makes it harder to get false
-- positives.
forall Shape S {

   scalar width = 4 -- line width

   list S.points = [ [-15,-20,-25], [15,-20,-25], [-15, 20,-25], [-15,-20, 25] ]
   list S.computedPoints = S.points
   list S.referencePoints = S.points

   vec3 S.c0 = ( S.computedPoints[0][0], S.computedPoints[0][1] )
   vec3 S.c1 = ( S.computedPoints[1][0], S.computedPoints[1][1] )
   vec3 S.c2 = ( S.computedPoints[2][0], S.computedPoints[2][1] )
   vec3 S.c3 = ( S.computedPoints[3][0], S.computedPoints[3][1] )

   vec3 S.r0 = ( S.referencePoints[0][0], S.referencePoints[0][1] )
   vec3 S.r1 = ( S.referencePoints[1][0], S.referencePoints[1][1] )
   vec3 S.r2 = ( S.referencePoints[2][0], S.referencePoints[2][1] )
   vec3 S.r3 = ( S.referencePoints[3][0], S.referencePoints[3][1] )

   color red = #f00
   color green = #0e0

   shape S.reference0 = Polyline {
      points: [ S.r0, S.r1 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference1 = Polyline {
      points: [ S.r0, S.r2 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference2 = Polyline {
      points: [ S.r0, S.r3 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference3 = Polyline {
      points: [ S.r1, S.r2 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference4 = Polyline {
      points: [ S.r2, S.r3 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference5 = Polyline {
      points: [ S.r3, S.r1 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference = Group {
      shapes: [ S.reference0, S.reference1, S.reference2, S.reference3, S.reference4, S.reference5 ]
   }
   layer S.reference above canvas.background

   shape S.computed0 = Polyline {
      points: [ S.c0, S.c1 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed1 = Polyline {
      points: [ S.c0, S.c2 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed2 = Polyline {
      points: [ S.c0, S.c3 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed3 = Polyline {
      points: [ S.c1, S.c2 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed4 = Polyline {
      points: [ S.c2, S.c3 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed5 = Polyline {
      points: [ S.c3, S.c1 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed = Group {
      shapes: [ S.computed0, S.computed1, S.computed2, S.computed3, S.computed4, S.computed5 ]
   }
   layer S.computed above S.reference
}

-- For each function in the matrix library, compare the result provided
-- by Penrose with the result computed by equivalent functions in an
-- external reference program.  In this case, all reference values were
-- computed in Mathematica, via the notebook \`Reference3D.nb\`.

forall Shape S
where identity3d(S) {
   mat3x3 A = identity(3)
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [ (-15,-20,-25), ( 15,-20,-25), (-15, 20,-25), (-15,-20, 25) ]
}

forall Shape S
where diagonal3d(S) {
   mat3x3 A = diagonal( (1.2, -0.9, 1.1) )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-18.,18.,-27.5),(18.,18.,-27.5),(-18.,-18.,-27.5),(-18.,18.,27.5)]
}

forall Shape S
where inverse3d(S) {
   mat3x3 A = [[0.8, 0.1, 0.2], [0.1, 0.5, -0.4], [0.3, -1.4, -1.3]]
   override S.computedPoints = matrixMultiplyList( inverse(A), S.points )
   override S.referencePoints = [(-24.0488,-13.0244,27.7073),(11.3659,-13.3171,36.1951),(-18.1951,29.9024,-17.1707),(-17.2195,-29.6098,8.68293)]
}

forall Shape S
where outerProduct3d(S) {
   mat3x3 A = outerProduct( [-.2, .3, .4], [.3, .4, .1] ) + outerProduct( [1.1, -1.0, .9], [.8, .7, .6] ) + outerProduct( [.4, .5, -.2], [-.2, .2, .1] )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-43.5,34.75,-42.2),(-21.3,10.45,-15.8),(-12.7,15.55,-12.2),(-9.5,8.75,-14.2)]
}

forall Shape S
where crossProductMatrix(S) {
   vec3 u = (.9, -1.2, .7)
   mat3x3 A = crossProductMatrix(u)
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(44.,12.,-36.),(44.,33.,0.),(16.,12.,0.),(-16.,-33.,-36.)]
}

forall Shape S
where rotate3d(S) {
   scalar \u03B8 = 12.3
   vec3 u = (1, 1, 1)/sqrt(3)
   mat3x3 A = rotate3d( \u03B8, u )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-14.4165,-21.5198,-24.0638),(14.8782,-25.7264,-19.1518),(-7.86714,17.5398,-29.6726),(-21.4275,-13.3331,24.7606)]
}

forall Shape S
where rotate3dh(S) {
   scalar \u03B8 = 3.21
   vec3 u = (1, -1, 1)/sqrt(3)
   mat3x3 A = rotate3dh( \u03B8, u )
   override S.computedPoints = fromHomogeneousList( matrixMultiplyList( A, toHomogeneousList(S.points) ))
   override S.referencePoints = [(-0.128713,32.8763,13.005),(-10.0819,11.7158,31.7977),(-25.1856,19.6054,-15.209),(35.1388,1.55518,-3.58367)]
}

forall Shape S
where scale3d(S) {
   mat3x3 A = scale3d( 1.3, -1.2, -1.1 )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-19.5,24.,27.5),(19.5,24.,27.5),(-19.5,-24.,27.5),(-19.5,24.,-27.5)]
}

forall Shape S
where shear3d(S) {
   mat3x3 A = shear3d( [.5, .3, -.1], [.2, 1.3, 1.0] )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-42.,-36.2,-19.6),(-9.,-34.4,-20.2),(-16.,19.4,-24.8),(-17.,-21.2,25.4)]
}

forall Shape S
where translate3dh(S) {
   mat3x3 A = translate3dh( 12.3, -23.4, 4.32 )
   override S.computedPoints = fromHomogeneousList( matrixMultiplyList( A, toHomogeneousList( S.points ) ))
   override S.referencePoints = [(-2.7,-43.4,-20.68),(27.3,-43.4,-20.68),(-2.7,-3.4,-20.68),(-2.7,-43.4,29.32)]
}

forall Shape S
where matrix3d(S) {
   mat3x3 A = [ [1.2, .2, .1], [.1, 1.2, -.3], [.2, -1.2, -.4] ]
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-24.5,-18.,31.),(11.5,-15.,37.),(-16.5,30.,-17.),(-19.5,-33.,11.)]
}
		</sty>
	<dsl>type Shape
predicate identity3d( Shape S )
predicate diagonal3d( Shape S )
predicate inverse3d( Shape S )
predicate outerProduct3d( Shape S )
predicate crossProductMatrix( Shape S )
predicate rotate3d( Shape S )
predicate rotate3dh( Shape S )
predicate scale3d( Shape S )
predicate shear3d( Shape S )
predicate translate3dh( Shape S )
predicate matrix3d( Shape S )
		</dsl>
</penrose>
<rect x="109.7260284" y="1.6891892" class="st0" width="100" height="100"/>
<g>
	<line class="st1" x1="145.3095245" y1="73.2089844" x2="174.6042328" y2="77.4155884"/>
	<line class="st1" x1="145.3095245" y1="73.2089844" x2="151.8588867" y2="34.1493912"/>
	<line class="st1" x1="145.3095245" y1="73.2089844" x2="138.2985229" y2="65.0222855"/>
	<line class="st1" x1="174.6042328" y1="77.4155884" x2="151.8588867" y2="34.1493912"/>
	<line class="st1" x1="151.8588867" y1="34.1493912" x2="138.2985229" y2="65.0222855"/>
	<line class="st1" x1="138.2985229" y1="65.0222855" x2="174.6042328" y2="77.4155884"/>
</g>
<g>
	<line class="st2" x1="145.229187" y1="72.9286804" x2="174.7628479" y2="76.4139862"/>
	<line class="st2" x1="145.229187" y1="72.9286804" x2="150.4980469" y2="33.5504646"/>
	<line class="st2" x1="145.229187" y1="72.9286804" x2="139.4203491" y2="66.3426056"/>
	<line class="st2" x1="174.7628479" y1="76.4139862" x2="150.4980469" y2="33.5504646"/>
	<line class="st2" x1="150.4980469" y1="33.5504646" x2="139.4203491" y2="66.3426056"/>
	<line class="st2" x1="139.4203491" y1="66.3426056" x2="174.7628479" y2="76.4139862"/>
</g>
<penrose>
	<croppedViewBox>0 0 100 100</croppedViewBox>
	<version>0.1</version>
	<variation>FudgesicleWoodpecker2520</variation>
	<sub>Shape S
rotate3d(S)</sub>
	<sty>canvas {
   width = 100
   height = 100

   shape background = Rectangle {
      fillColor: #0f0
      center: (0,0)
      width: width
      height: height
   }
}

global {
   -- Due to rasterization effects (such as anti-aliasing),
   -- the reference and computed shapes may not match exactly.
   -- For instance, even two shapes with identical coordinates
   -- but different colors may yield a &quot;fringing&quot; around the
   -- border.  This parameter determines a &quot;fudge factor&quot; that
   -- is used to compare shapes, e.g., by drawing a small
   -- margin around the computed shape.
   scalar tolerance = 0.25
}

-- Use an asymmetric tetrahedron as the test shape, since
-- it does not have any Euclidean symmetries (i.e., it
-- cannot be mapped to itself by rotation, reflection,
-- translation, etc.), which makes it harder to get false
-- positives.
forall Shape S {

   scalar width = 4 -- line width

   list S.points = [ [-15,-20,-25], [15,-20,-25], [-15, 20,-25], [-15,-20, 25] ]
   list S.computedPoints = S.points
   list S.referencePoints = S.points

   vec3 S.c0 = ( S.computedPoints[0][0], S.computedPoints[0][1] )
   vec3 S.c1 = ( S.computedPoints[1][0], S.computedPoints[1][1] )
   vec3 S.c2 = ( S.computedPoints[2][0], S.computedPoints[2][1] )
   vec3 S.c3 = ( S.computedPoints[3][0], S.computedPoints[3][1] )

   vec3 S.r0 = ( S.referencePoints[0][0], S.referencePoints[0][1] )
   vec3 S.r1 = ( S.referencePoints[1][0], S.referencePoints[1][1] )
   vec3 S.r2 = ( S.referencePoints[2][0], S.referencePoints[2][1] )
   vec3 S.r3 = ( S.referencePoints[3][0], S.referencePoints[3][1] )

   color red = #f00
   color green = #0e0

   shape S.reference0 = Polyline {
      points: [ S.r0, S.r1 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference1 = Polyline {
      points: [ S.r0, S.r2 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference2 = Polyline {
      points: [ S.r0, S.r3 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference3 = Polyline {
      points: [ S.r1, S.r2 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference4 = Polyline {
      points: [ S.r2, S.r3 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference5 = Polyline {
      points: [ S.r3, S.r1 ]
      strokeColor: red
      strokeWidth: width
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.reference = Group {
      shapes: [ S.reference0, S.reference1, S.reference2, S.reference3, S.reference4, S.reference5 ]
   }
   layer S.reference above canvas.background

   shape S.computed0 = Polyline {
      points: [ S.c0, S.c1 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed1 = Polyline {
      points: [ S.c0, S.c2 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed2 = Polyline {
      points: [ S.c0, S.c3 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed3 = Polyline {
      points: [ S.c1, S.c2 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed4 = Polyline {
      points: [ S.c2, S.c3 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed5 = Polyline {
      points: [ S.c3, S.c1 ]
      strokeColor: green
      strokeWidth: width + global.tolerance
      strokeLinecap: &quot;round&quot;
      ensureOnCanvas: false
   }
   shape S.computed = Group {
      shapes: [ S.computed0, S.computed1, S.computed2, S.computed3, S.computed4, S.computed5 ]
   }
   layer S.computed above S.reference
}

-- For each function in the matrix library, compare the result provided
-- by Penrose with the result computed by equivalent functions in an
-- external reference program.  In this case, all reference values were
-- computed in Mathematica, via the notebook \`Reference3D.nb\`.

forall Shape S
where identity3d(S) {
   mat3x3 A = identity(3)
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [ (-15,-20,-25), ( 15,-20,-25), (-15, 20,-25), (-15,-20, 25) ]
}

forall Shape S
where diagonal3d(S) {
   mat3x3 A = diagonal( (1.2, -0.9, 1.1) )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-18.,18.,-27.5),(18.,18.,-27.5),(-18.,-18.,-27.5),(-18.,18.,27.5)]
}

forall Shape S
where inverse3d(S) {
   mat3x3 A = [[0.8, 0.1, 0.2], [0.1, 0.5, -0.4], [0.3, -1.4, -1.3]]
   override S.computedPoints = matrixMultiplyList( inverse(A), S.points )
   override S.referencePoints = [(-24.0488,-13.0244,27.7073),(11.3659,-13.3171,36.1951),(-18.1951,29.9024,-17.1707),(-17.2195,-29.6098,8.68293)]
}

forall Shape S
where outerProduct3d(S) {
   mat3x3 A = outerProduct( [-.2, .3, .4], [.3, .4, .1] ) + outerProduct( [1.1, -1.0, .9], [.8, .7, .6] ) + outerProduct( [.4, .5, -.2], [-.2, .2, .1] )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-43.5,34.75,-42.2),(-21.3,10.45,-15.8),(-12.7,15.55,-12.2),(-9.5,8.75,-14.2)]
}

forall Shape S
where crossProductMatrix(S) {
   vec3 u = (.9, -1.2, .7)
   mat3x3 A = crossProductMatrix(u)
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(44.,12.,-36.),(44.,33.,0.),(16.,12.,0.),(-16.,-33.,-36.)]
}

forall Shape S
where rotate3d(S) {
   scalar \u03B8 = 12.35
   vec3 u = (1, 1, 1)/sqrt(3)
   mat3x3 A = rotate3d( \u03B8, u )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-14.4165,-21.5198,-24.0638),(14.8782,-25.7264,-19.1518),(-7.86714,17.5398,-29.6726),(-21.4275,-13.3331,24.7606)]
}

forall Shape S
where rotate3dh(S) {
   scalar \u03B8 = 3.21
   vec3 u = (1, -1, 1)/sqrt(3)
   mat3x3 A = rotate3dh( \u03B8, u )
   override S.computedPoints = fromHomogeneousList( matrixMultiplyList( A, toHomogeneousList(S.points) ))
   override S.referencePoints = [(-0.128713,32.8763,13.005),(-10.0819,11.7158,31.7977),(-25.1856,19.6054,-15.209),(35.1388,1.55518,-3.58367)]
}

forall Shape S
where scale3d(S) {
   mat3x3 A = scale3d( 1.3, -1.2, -1.1 )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-19.5,24.,27.5),(19.5,24.,27.5),(-19.5,-24.,27.5),(-19.5,24.,-27.5)]
}

forall Shape S
where shear3d(S) {
   mat3x3 A = shear3d( [.5, .3, -.1], [.2, 1.3, 1.0] )
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-42.,-36.2,-19.6),(-9.,-34.4,-20.2),(-16.,19.4,-24.8),(-17.,-21.2,25.4)]
}

forall Shape S
where translate3dh(S) {
   mat3x3 A = translate3dh( 12.3, -23.4, 4.32 )
   override S.computedPoints = fromHomogeneousList( matrixMultiplyList( A, toHomogeneousList( S.points ) ))
   override S.referencePoints = [(-2.7,-43.4,-20.68),(27.3,-43.4,-20.68),(-2.7,-3.4,-20.68),(-2.7,-43.4,29.32)]
}

forall Shape S
where matrix3d(S) {
   mat3x3 A = [ [1.2, .2, .1], [.1, 1.2, -.3], [.2, -1.2, -.4] ]
   override S.computedPoints = matrixMultiplyList( A, S.points )
   override S.referencePoints = [(-24.5,-18.,31.),(11.5,-15.,37.),(-16.5,30.,-17.),(-19.5,-33.,11.)]
}
		</sty>
	<dsl>type Shape
predicate identity3d( Shape S )
predicate diagonal3d( Shape S )
predicate inverse3d( Shape S )
predicate outerProduct3d( Shape S )
predicate crossProductMatrix( Shape S )
predicate rotate3d( Shape S )
predicate rotate3dh( Shape S )
predicate scale3d( Shape S )
predicate shear3d( Shape S )
predicate translate3dh( Shape S )
predicate matrix3d( Shape S )
		</dsl>
</penrose>
<g>
	<text transform="matrix(1 0 0 1 44.0565987 113.2884827)" class="st3 st4 st5">pass</text>
	<text transform="matrix(1 0 0 1 44.0565987 113.2884827)" class="st6 st4 st5">pass</text>
	<text transform="matrix(1 0 0 1 44.0565987 113.2884827)" class="st7 st4 st5">pass</text>
</g>
<g>
	<text transform="matrix(1 0 0 1 152.1843109 113.2884827)" class="st3 st4 st5">fail</text>
	<text transform="matrix(1 0 0 1 152.1843109 113.2884827)" class="st6 st4 st5">fail</text>
	<text transform="matrix(1 0 0 1 152.1843109 113.2884827)" class="st7 st4 st5">fail</text>
</g>
</svg>
`;export{e as default};
