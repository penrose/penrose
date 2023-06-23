import{m as n}from"./resolver-52f68c7c.js";import"./iframe-b9a20e36.js";const s=`Curve c1

inSphere(c1)
hasShadow(c1)
`,e=n("curve-examples/space-curves"),a=`canvas {
  width = 800
  height = 1000
}

layout = [manifolds, maps]

settings {
  lineThickness = 2.5
  arrowThickness = 1.5
  blobbiness = 0.1
  pi = 3.14159
  fontSize = "15px"
  blobDistance = 300
  sphereRadius = 100
  curveLength = 900
}

global {

  -- Ground plane coordinates in 3D
  scalar planeSize = 300 -- plane size
  scalar planeHeight = -350 -- plane height

  -- Use a simple pinhole camera model, where the
  -- only camera parameter is the distance along Z
  scalar cZ_ = ? -- camera z coordinate

  ensure -1900 < cZ_
  ensure cZ_ < -1100

  scalar cZ = min(-1000, max(cZ_, -2000))

  -- Corner coordinates of the global ground plane
  vec3 Kq00 = ( -planeSize, planeHeight, -planeSize )
  vec3 Kq10 = (  planeSize, planeHeight, -planeSize )
  vec3 Kq01 = ( -planeSize, planeHeight,  planeSize )
  vec3 Kq11 = (  planeSize, planeHeight,  planeSize )

  -- Apply a random rotation to the ground plane
  -- (Note that we could also apply this rotation to the triangle
  -- vertices, but since they're sampled randomly, it wouldn't
  -- really change the appearance of the kinds of diagrams we sample).
  scalar θ = ?
  vec3 KQ00 = ( Kq00[0]*cos(θ) + Kq00[2]*sin(θ), Kq00[1], Kq00[2]*cos(θ) - Kq00[0]*sin(θ) )
  vec3 KQ10 = ( Kq10[0]*cos(θ) + Kq10[2]*sin(θ), Kq10[1], Kq10[2]*cos(θ) - Kq10[0]*sin(θ) )
  vec3 KQ01 = ( Kq01[0]*cos(θ) + Kq01[2]*sin(θ), Kq01[1], Kq01[2]*cos(θ) - Kq01[0]*sin(θ) )
  vec3 KQ11 = ( Kq11[0]*cos(θ) + Kq11[2]*sin(θ), Kq11[1], Kq11[2]*cos(θ) - Kq11[0]*sin(θ) )

  -- Perform perspective projection on 3D coordinates to get 2D coordinates p
  vec2 Kp00 = canvas.width * (KQ00[0],KQ00[1] - 0)/(KQ00[2] - cZ)
  vec2 Kp10 = canvas.width * (KQ10[0],KQ10[1] - 0)/(KQ10[2] - cZ)
  vec2 Kp01 = canvas.width * (KQ01[0],KQ01[1] - 0)/(KQ01[2] - cZ)
  vec2 Kp11 = canvas.width * (KQ11[0],KQ11[1] - 0)/(KQ11[2] - cZ)

  -- Draw polygon using projected 2D coordinates p
  shape groundPlane = Polygon {
    points: ( Kp00, Kp10, Kp11, Kp01 )
    width: canvas.width
    height: canvas.height
    fillColor: rgba(0.5,0.5,.9,0.2)
    strokeColor: rgba(.7,0.7,1,0)
    strokeWidth: .5
    ensureOnCanvas: false
  }

  vec3 center = ( 0, 0, 0 )
  vec2 scenter = canvas.width * (center[0], center[1]) / (center[2] - global.cZ)

  shape sphere = Circle {
    center: scenter
    r: -1.01 * canvas.width * settings.sphereRadius / global.cZ
    fillColor: #ac2b2b33
  }

}


forall Curve c {
  vec3 c.p0 = (?, ?, ?)
  vec3 c.p1 = (?, ?, ?)
  vec3 c.p2 = (?, ?, ?)
  vec3 c.p3 = (?, ?, ?)
  vec3 c.p4 = (?, ?, ?)
  vec3 c.p5 = (?, ?, ?)
  vec3 c.p6 = (?, ?, ?)
  vec3 c.p7 = (?, ?, ?)
  vec3 c.p8 = (?, ?, ?)
  vec3 c.p9 = (?, ?, ?)
  vec3 c.p10 = (?, ?, ?)
  vec3 c.p11 = (?, ?, ?)
  vec3 c.p12 = (?, ?, ?)
  vec3 c.p13 = (?, ?, ?)
  vec3 c.p14 = (?, ?, ?)
  vec3 c.p15 = (?, ?, ?)
  vec3 c.p16 = (?, ?, ?)
  vec3 c.p17 = (?, ?, ?)
  vec3 c.p18 = (?, ?, ?)
  vec3 c.p19 = (?, ?, ?)
  vec3 c.p20 = (?, ?, ?)
  vec3 c.p21 = (?, ?, ?)
  vec3 c.p22 = (?, ?, ?)
  vec3 c.p23 = (?, ?, ?)
  vec3 c.p24 = (?, ?, ?)
  vec3 c.p25 = (?, ?, ?)
  vec3 c.p26 = (?, ?, ?)
  vec3 c.p27 = (?, ?, ?)
  vec3 c.p28 = (?, ?, ?)
  vec3 c.p29 = (?, ?, ?)
  vec3 c.p30 = (?, ?, ?)
  vec3 c.p31 = (?, ?, ?)
  vec3 c.p32 = (?, ?, ?)
  vec3 c.p33 = (?, ?, ?)
  vec3 c.p34 = (?, ?, ?)
  vec3 c.p35 = (?, ?, ?)
  vec3 c.p36 = (?, ?, ?)
  vec3 c.p37 = (?, ?, ?)
  vec3 c.p38 = (?, ?, ?)
  vec3 c.p39 = (?, ?, ?)
  vec3 c.p40 = (?, ?, ?)
  vec3 c.p41 = (?, ?, ?)
  vec3 c.p42 = (?, ?, ?)
  vec3 c.p43 = (?, ?, ?)
  vec3 c.p44 = (?, ?, ?)
  vec3 c.p45 = (?, ?, ?)
  vec3 c.p46 = (?, ?, ?)
  vec3 c.p47 = (?, ?, ?)
  vec3 c.p48 = (?, ?, ?)
  vec3 c.p49 = (?, ?, ?)
  
  vec3 q0 = ( c.p0[0] * cos(global.θ) + c.p0[2]*sin(global.θ), c.p0[1], c.p0[2] * cos(global.θ) - c.p0[0] * sin(global.θ) )
  vec3 q1 = ( c.p1[0] * cos(global.θ) + c.p1[2]*sin(global.θ), c.p1[1], c.p1[2] * cos(global.θ) - c.p1[0] * sin(global.θ) )
  vec3 q2 = ( c.p2[0] * cos(global.θ) + c.p2[2]*sin(global.θ), c.p2[1], c.p2[2] * cos(global.θ) - c.p2[0] * sin(global.θ) )
  vec3 q3 = ( c.p3[0] * cos(global.θ) + c.p3[2]*sin(global.θ), c.p3[1], c.p3[2] * cos(global.θ) - c.p3[0] * sin(global.θ) )
  vec3 q4 = ( c.p4[0] * cos(global.θ) + c.p4[2]*sin(global.θ), c.p4[1], c.p4[2] * cos(global.θ) - c.p4[0] * sin(global.θ) )
  vec3 q5 = ( c.p5[0] * cos(global.θ) + c.p5[2]*sin(global.θ), c.p5[1], c.p5[2] * cos(global.θ) - c.p5[0] * sin(global.θ) )
  vec3 q6 = ( c.p6[0] * cos(global.θ) + c.p6[2]*sin(global.θ), c.p6[1], c.p6[2] * cos(global.θ) - c.p6[0] * sin(global.θ) )
  vec3 q7 = ( c.p7[0] * cos(global.θ) + c.p7[2]*sin(global.θ), c.p7[1], c.p7[2] * cos(global.θ) - c.p7[0] * sin(global.θ) )
  vec3 q8 = ( c.p8[0] * cos(global.θ) + c.p8[2]*sin(global.θ), c.p8[1], c.p8[2] * cos(global.θ) - c.p8[0] * sin(global.θ) )
  vec3 q9 = ( c.p9[0] * cos(global.θ) + c.p9[2]*sin(global.θ), c.p9[1], c.p9[2] * cos(global.θ) - c.p9[0] * sin(global.θ) )
  vec3 q10 = ( c.p10[0] * cos(global.θ) + c.p10[2]*sin(global.θ), c.p10[1], c.p10[2] * cos(global.θ) - c.p10[0] * sin(global.θ) )
  vec3 q11 = ( c.p11[0] * cos(global.θ) + c.p11[2]*sin(global.θ), c.p11[1], c.p11[2] * cos(global.θ) - c.p11[0] * sin(global.θ) )
  vec3 q12 = ( c.p12[0] * cos(global.θ) + c.p12[2]*sin(global.θ), c.p12[1], c.p12[2] * cos(global.θ) - c.p12[0] * sin(global.θ) )
  vec3 q13 = ( c.p13[0] * cos(global.θ) + c.p13[2]*sin(global.θ), c.p13[1], c.p13[2] * cos(global.θ) - c.p13[0] * sin(global.θ) )
  vec3 q14 = ( c.p14[0] * cos(global.θ) + c.p14[2]*sin(global.θ), c.p14[1], c.p14[2] * cos(global.θ) - c.p14[0] * sin(global.θ) )
  vec3 q15 = ( c.p15[0] * cos(global.θ) + c.p15[2]*sin(global.θ), c.p15[1], c.p15[2] * cos(global.θ) - c.p15[0] * sin(global.θ) )
  vec3 q16 = ( c.p16[0] * cos(global.θ) + c.p16[2]*sin(global.θ), c.p16[1], c.p16[2] * cos(global.θ) - c.p16[0] * sin(global.θ) )
  vec3 q17 = ( c.p17[0] * cos(global.θ) + c.p17[2]*sin(global.θ), c.p17[1], c.p17[2] * cos(global.θ) - c.p17[0] * sin(global.θ) )
  vec3 q18 = ( c.p18[0] * cos(global.θ) + c.p18[2]*sin(global.θ), c.p18[1], c.p18[2] * cos(global.θ) - c.p18[0] * sin(global.θ) )
  vec3 q19 = ( c.p19[0] * cos(global.θ) + c.p19[2]*sin(global.θ), c.p19[1], c.p19[2] * cos(global.θ) - c.p19[0] * sin(global.θ) )
  vec3 q20 = ( c.p20[0] * cos(global.θ) + c.p20[2]*sin(global.θ), c.p20[1], c.p20[2] * cos(global.θ) - c.p20[0] * sin(global.θ) )
  vec3 q21 = ( c.p21[0] * cos(global.θ) + c.p21[2]*sin(global.θ), c.p21[1], c.p21[2] * cos(global.θ) - c.p21[0] * sin(global.θ) )
  vec3 q22 = ( c.p22[0] * cos(global.θ) + c.p22[2]*sin(global.θ), c.p22[1], c.p22[2] * cos(global.θ) - c.p22[0] * sin(global.θ) )
  vec3 q23 = ( c.p23[0] * cos(global.θ) + c.p23[2]*sin(global.θ), c.p23[1], c.p23[2] * cos(global.θ) - c.p23[0] * sin(global.θ) )
  vec3 q24 = ( c.p24[0] * cos(global.θ) + c.p24[2]*sin(global.θ), c.p24[1], c.p24[2] * cos(global.θ) - c.p24[0] * sin(global.θ) )
  vec3 q25 = ( c.p25[0] * cos(global.θ) + c.p25[2]*sin(global.θ), c.p25[1], c.p25[2] * cos(global.θ) - c.p25[0] * sin(global.θ) )
  vec3 q26 = ( c.p26[0] * cos(global.θ) + c.p26[2]*sin(global.θ), c.p26[1], c.p26[2] * cos(global.θ) - c.p26[0] * sin(global.θ) )
  vec3 q27 = ( c.p27[0] * cos(global.θ) + c.p27[2]*sin(global.θ), c.p27[1], c.p27[2] * cos(global.θ) - c.p27[0] * sin(global.θ) )
  vec3 q28 = ( c.p28[0] * cos(global.θ) + c.p28[2]*sin(global.θ), c.p28[1], c.p28[2] * cos(global.θ) - c.p28[0] * sin(global.θ) )
  vec3 q29 = ( c.p29[0] * cos(global.θ) + c.p29[2]*sin(global.θ), c.p29[1], c.p29[2] * cos(global.θ) - c.p29[0] * sin(global.θ) )
  vec3 q30 = ( c.p30[0] * cos(global.θ) + c.p30[2]*sin(global.θ), c.p30[1], c.p30[2] * cos(global.θ) - c.p30[0] * sin(global.θ) )
  vec3 q31 = ( c.p31[0] * cos(global.θ) + c.p31[2]*sin(global.θ), c.p31[1], c.p31[2] * cos(global.θ) - c.p31[0] * sin(global.θ) )
  vec3 q32 = ( c.p32[0] * cos(global.θ) + c.p32[2]*sin(global.θ), c.p32[1], c.p32[2] * cos(global.θ) - c.p32[0] * sin(global.θ) )
  vec3 q33 = ( c.p33[0] * cos(global.θ) + c.p33[2]*sin(global.θ), c.p33[1], c.p33[2] * cos(global.θ) - c.p33[0] * sin(global.θ) )
  vec3 q34 = ( c.p34[0] * cos(global.θ) + c.p34[2]*sin(global.θ), c.p34[1], c.p34[2] * cos(global.θ) - c.p34[0] * sin(global.θ) )
  vec3 q35 = ( c.p35[0] * cos(global.θ) + c.p35[2]*sin(global.θ), c.p35[1], c.p35[2] * cos(global.θ) - c.p35[0] * sin(global.θ) )
  vec3 q36 = ( c.p36[0] * cos(global.θ) + c.p36[2]*sin(global.θ), c.p36[1], c.p36[2] * cos(global.θ) - c.p36[0] * sin(global.θ) )
  vec3 q37 = ( c.p37[0] * cos(global.θ) + c.p37[2]*sin(global.θ), c.p37[1], c.p37[2] * cos(global.θ) - c.p37[0] * sin(global.θ) )
  vec3 q38 = ( c.p38[0] * cos(global.θ) + c.p38[2]*sin(global.θ), c.p38[1], c.p38[2] * cos(global.θ) - c.p38[0] * sin(global.θ) )
  vec3 q39 = ( c.p39[0] * cos(global.θ) + c.p39[2]*sin(global.θ), c.p39[1], c.p39[2] * cos(global.θ) - c.p39[0] * sin(global.θ) )
  vec3 q40 = ( c.p40[0] * cos(global.θ) + c.p40[2]*sin(global.θ), c.p40[1], c.p40[2] * cos(global.θ) - c.p40[0] * sin(global.θ) )
  vec3 q41 = ( c.p41[0] * cos(global.θ) + c.p41[2]*sin(global.θ), c.p41[1], c.p41[2] * cos(global.θ) - c.p41[0] * sin(global.θ) )
  vec3 q42 = ( c.p42[0] * cos(global.θ) + c.p42[2]*sin(global.θ), c.p42[1], c.p42[2] * cos(global.θ) - c.p42[0] * sin(global.θ) )
  vec3 q43 = ( c.p43[0] * cos(global.θ) + c.p43[2]*sin(global.θ), c.p43[1], c.p43[2] * cos(global.θ) - c.p43[0] * sin(global.θ) )
  vec3 q44 = ( c.p44[0] * cos(global.θ) + c.p44[2]*sin(global.θ), c.p44[1], c.p44[2] * cos(global.θ) - c.p44[0] * sin(global.θ) )
  vec3 q45 = ( c.p45[0] * cos(global.θ) + c.p45[2]*sin(global.θ), c.p45[1], c.p45[2] * cos(global.θ) - c.p45[0] * sin(global.θ) )
  vec3 q46 = ( c.p46[0] * cos(global.θ) + c.p46[2]*sin(global.θ), c.p46[1], c.p46[2] * cos(global.θ) - c.p46[0] * sin(global.θ) )
  vec3 q47 = ( c.p47[0] * cos(global.θ) + c.p47[2]*sin(global.θ), c.p47[1], c.p47[2] * cos(global.θ) - c.p47[0] * sin(global.θ) )
  vec3 q48 = ( c.p48[0] * cos(global.θ) + c.p48[2]*sin(global.θ), c.p48[1], c.p48[2] * cos(global.θ) - c.p48[0] * sin(global.θ) )
  vec3 q49 = ( c.p49[0] * cos(global.θ) + c.p49[2]*sin(global.θ), c.p49[1], c.p49[2] * cos(global.θ) - c.p49[0] * sin(global.θ) )

  vec2 r0 = canvas.width * (q0[0], q0[1]) / (q0[2] - global.cZ)
  vec2 r1 = canvas.width * (q1[0], q1[1]) / (q1[2] - global.cZ)
  vec2 r2 = canvas.width * (q2[0], q2[1]) / (q2[2] - global.cZ)
  vec2 r3 = canvas.width * (q3[0], q3[1]) / (q3[2] - global.cZ)
  vec2 r4 = canvas.width * (q4[0], q4[1]) / (q4[2] - global.cZ)
  vec2 r5 = canvas.width * (q5[0], q5[1]) / (q5[2] - global.cZ)
  vec2 r6 = canvas.width * (q6[0], q6[1]) / (q6[2] - global.cZ)
  vec2 r7 = canvas.width * (q7[0], q7[1]) / (q7[2] - global.cZ)
  vec2 r8 = canvas.width * (q8[0], q8[1]) / (q8[2] - global.cZ)
  vec2 r9 = canvas.width * (q9[0], q9[1]) / (q9[2] - global.cZ)
  vec2 r10 = canvas.width * (q10[0], q10[1]) / (q10[2] - global.cZ)
  vec2 r11 = canvas.width * (q11[0], q11[1]) / (q11[2] - global.cZ)
  vec2 r12 = canvas.width * (q12[0], q12[1]) / (q12[2] - global.cZ)
  vec2 r13 = canvas.width * (q13[0], q13[1]) / (q13[2] - global.cZ)
  vec2 r14 = canvas.width * (q14[0], q14[1]) / (q14[2] - global.cZ)
  vec2 r15 = canvas.width * (q15[0], q15[1]) / (q15[2] - global.cZ)
  vec2 r16 = canvas.width * (q16[0], q16[1]) / (q16[2] - global.cZ)
  vec2 r17 = canvas.width * (q17[0], q17[1]) / (q17[2] - global.cZ)
  vec2 r18 = canvas.width * (q18[0], q18[1]) / (q18[2] - global.cZ)
  vec2 r19 = canvas.width * (q19[0], q19[1]) / (q19[2] - global.cZ)
  vec2 r20 = canvas.width * (q20[0], q20[1]) / (q20[2] - global.cZ)
  vec2 r21 = canvas.width * (q21[0], q21[1]) / (q21[2] - global.cZ)
  vec2 r22 = canvas.width * (q22[0], q22[1]) / (q22[2] - global.cZ)
  vec2 r23 = canvas.width * (q23[0], q23[1]) / (q23[2] - global.cZ)
  vec2 r24 = canvas.width * (q24[0], q24[1]) / (q24[2] - global.cZ)
  vec2 r25 = canvas.width * (q25[0], q25[1]) / (q25[2] - global.cZ)
  vec2 r26 = canvas.width * (q26[0], q26[1]) / (q26[2] - global.cZ)
  vec2 r27 = canvas.width * (q27[0], q27[1]) / (q27[2] - global.cZ)
  vec2 r28 = canvas.width * (q28[0], q28[1]) / (q28[2] - global.cZ)
  vec2 r29 = canvas.width * (q29[0], q29[1]) / (q29[2] - global.cZ)
  vec2 r30 = canvas.width * (q30[0], q30[1]) / (q30[2] - global.cZ)
  vec2 r31 = canvas.width * (q31[0], q31[1]) / (q31[2] - global.cZ)
  vec2 r32 = canvas.width * (q32[0], q32[1]) / (q32[2] - global.cZ)
  vec2 r33 = canvas.width * (q33[0], q33[1]) / (q33[2] - global.cZ)
  vec2 r34 = canvas.width * (q34[0], q34[1]) / (q34[2] - global.cZ)
  vec2 r35 = canvas.width * (q35[0], q35[1]) / (q35[2] - global.cZ)
  vec2 r36 = canvas.width * (q36[0], q36[1]) / (q36[2] - global.cZ)
  vec2 r37 = canvas.width * (q37[0], q37[1]) / (q37[2] - global.cZ)
  vec2 r38 = canvas.width * (q38[0], q38[1]) / (q38[2] - global.cZ)
  vec2 r39 = canvas.width * (q39[0], q39[1]) / (q39[2] - global.cZ)
  vec2 r40 = canvas.width * (q40[0], q40[1]) / (q40[2] - global.cZ)
  vec2 r41 = canvas.width * (q41[0], q41[1]) / (q41[2] - global.cZ)
  vec2 r42 = canvas.width * (q42[0], q42[1]) / (q42[2] - global.cZ)
  vec2 r43 = canvas.width * (q43[0], q43[1]) / (q43[2] - global.cZ)
  vec2 r44 = canvas.width * (q44[0], q44[1]) / (q44[2] - global.cZ)
  vec2 r45 = canvas.width * (q45[0], q45[1]) / (q45[2] - global.cZ)
  vec2 r46 = canvas.width * (q46[0], q46[1]) / (q46[2] - global.cZ)
  vec2 r47 = canvas.width * (q47[0], q47[1]) / (q47[2] - global.cZ)
  vec2 r48 = canvas.width * (q48[0], q48[1]) / (q48[2] - global.cZ)
  vec2 r49 = canvas.width * (q49[0], q49[1]) / (q49[2] - global.cZ)

  c.points = [c.p0, c.p1, c.p2, c.p3, c.p4, c.p5, c.p6, c.p7, c.p8, c.p9, c.p10, c.p11, c.p12, c.p13, c.p14, c.p15, c.p16, c.p17, c.p18, c.p19, c.p20, c.p21, c.p22, c.p23, c.p24, c.p25, c.p26, c.p27, c.p28, c.p29, c.p30, c.p31, c.p32, c.p33, c.p34, c.p35, c.p36, c.p37, c.p38, c.p39, c.p40, c.p41, c.p42, c.p43, c.p44, c.p45, c.p46, c.p47, c.p48, c.p49]

  c.points2d = [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49]
  
  ensure isEquilateral(c.points, true)

  ensure perimeter(c.points, true) == settings.curveLength
  encourage maximal(signedArea(c.points2d, true))
  encourage elasticEnergy(c.points, true) == 0

  a = 0.4
  b = -0.002

  shape c.oline0 = Line{
	start: (r0[0], r0[1])
	end: (r1[0], r1[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q0[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline1 = Line{
	start: (r1[0], r1[1])
	end: (r2[0], r2[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q1[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline2 = Line{
	start: (r2[0], r2[1])
	end: (r3[0], r3[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q2[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline3 = Line{
	start: (r3[0], r3[1])
	end: (r4[0], r4[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q3[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline4 = Line{
	start: (r4[0], r4[1])
	end: (r5[0], r5[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q4[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline5 = Line{
	start: (r5[0], r5[1])
	end: (r6[0], r6[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q5[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline6 = Line{
	start: (r6[0], r6[1])
	end: (r7[0], r7[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q6[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline7 = Line{
	start: (r7[0], r7[1])
	end: (r8[0], r8[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q7[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline8 = Line{
	start: (r8[0], r8[1])
	end: (r9[0], r9[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q8[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline9 = Line{
	start: (r9[0], r9[1])
	end: (r10[0], r10[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q9[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline10 = Line{
	start: (r10[0], r10[1])
	end: (r11[0], r11[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q10[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline11 = Line{
	start: (r11[0], r11[1])
	end: (r12[0], r12[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q11[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline12 = Line{
	start: (r12[0], r12[1])
	end: (r13[0], r13[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q12[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline13 = Line{
	start: (r13[0], r13[1])
	end: (r14[0], r14[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q13[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline14 = Line{
	start: (r14[0], r14[1])
	end: (r15[0], r15[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q14[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline15 = Line{
	start: (r15[0], r15[1])
	end: (r16[0], r16[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q15[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline16 = Line{
	start: (r16[0], r16[1])
	end: (r17[0], r17[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q16[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline17 = Line{
	start: (r17[0], r17[1])
	end: (r18[0], r18[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q17[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline18 = Line{
	start: (r18[0], r18[1])
	end: (r19[0], r19[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q18[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline19 = Line{
	start: (r19[0], r19[1])
	end: (r20[0], r20[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q19[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline20 = Line{
	start: (r20[0], r20[1])
	end: (r21[0], r21[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q20[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline21 = Line{
	start: (r21[0], r21[1])
	end: (r22[0], r22[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q21[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline22 = Line{
	start: (r22[0], r22[1])
	end: (r23[0], r23[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q22[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline23 = Line{
	start: (r23[0], r23[1])
	end: (r24[0], r24[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q23[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline24 = Line{
	start: (r24[0], r24[1])
	end: (r25[0], r25[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q24[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline25 = Line{
	start: (r25[0], r25[1])
	end: (r26[0], r26[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q25[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline26 = Line{
	start: (r26[0], r26[1])
	end: (r27[0], r27[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q26[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline27 = Line{
	start: (r27[0], r27[1])
	end: (r28[0], r28[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q27[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline28 = Line{
	start: (r28[0], r28[1])
	end: (r29[0], r29[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q28[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline29 = Line{
	start: (r29[0], r29[1])
	end: (r30[0], r30[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q29[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline30 = Line{
	start: (r30[0], r30[1])
	end: (r31[0], r31[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q30[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline31 = Line{
	start: (r31[0], r31[1])
	end: (r32[0], r32[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q31[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline32 = Line{
	start: (r32[0], r32[1])
	end: (r33[0], r33[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q32[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline33 = Line{
	start: (r33[0], r33[1])
	end: (r34[0], r34[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q33[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline34 = Line{
	start: (r34[0], r34[1])
	end: (r35[0], r35[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q34[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline35 = Line{
	start: (r35[0], r35[1])
	end: (r36[0], r36[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q35[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline36 = Line{
	start: (r36[0], r36[1])
	end: (r37[0], r37[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q36[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline37 = Line{
	start: (r37[0], r37[1])
	end: (r38[0], r38[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q37[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline38 = Line{
	start: (r38[0], r38[1])
	end: (r39[0], r39[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q38[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline39 = Line{
	start: (r39[0], r39[1])
	end: (r40[0], r40[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q39[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline40 = Line{
	start: (r40[0], r40[1])
	end: (r41[0], r41[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q40[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline41 = Line{
	start: (r41[0], r41[1])
	end: (r42[0], r42[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q41[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline42 = Line{
	start: (r42[0], r42[1])
	end: (r43[0], r43[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q42[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline43 = Line{
	start: (r43[0], r43[1])
	end: (r44[0], r44[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q43[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline44 = Line{
	start: (r44[0], r44[1])
	end: (r45[0], r45[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q44[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline45 = Line{
	start: (r45[0], r45[1])
	end: (r46[0], r46[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q45[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline46 = Line{
	start: (r46[0], r46[1])
	end: (r47[0], r47[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q46[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline47 = Line{
	start: (r47[0], r47[1])
	end: (r48[0], r48[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q47[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline48 = Line{
	start: (r48[0], r48[1])
	end: (r49[0], r49[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q48[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.oline49 = Line{
	start: (r49[0], r49[1])
	end: (r0[0], r0[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * q49[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }


}

forall Curve c where inSphere(c) {
  scalar radius = settings.sphereRadius
  ensure norm(c.p0) < radius
  ensure norm(c.p1) < radius
  ensure norm(c.p2) < radius
  ensure norm(c.p3) < radius
  ensure norm(c.p4) < radius
  ensure norm(c.p5) < radius
  ensure norm(c.p6) < radius
  ensure norm(c.p7) < radius
  ensure norm(c.p8) < radius
  ensure norm(c.p9) < radius
  ensure norm(c.p10) < radius
  ensure norm(c.p11) < radius
  ensure norm(c.p12) < radius
  ensure norm(c.p13) < radius
  ensure norm(c.p14) < radius
  ensure norm(c.p15) < radius
  ensure norm(c.p16) < radius
  ensure norm(c.p17) < radius
  ensure norm(c.p18) < radius
  ensure norm(c.p19) < radius
  ensure norm(c.p20) < radius
  ensure norm(c.p21) < radius
  ensure norm(c.p22) < radius
  ensure norm(c.p23) < radius
  ensure norm(c.p24) < radius
  ensure norm(c.p25) < radius
  ensure norm(c.p26) < radius
  ensure norm(c.p27) < radius
  ensure norm(c.p28) < radius
  ensure norm(c.p29) < radius
  ensure norm(c.p30) < radius
  ensure norm(c.p31) < radius
  ensure norm(c.p32) < radius
  ensure norm(c.p33) < radius
  ensure norm(c.p34) < radius
  ensure norm(c.p35) < radius
  ensure norm(c.p36) < radius
  ensure norm(c.p37) < radius
  ensure norm(c.p38) < radius
  ensure norm(c.p39) < radius
  ensure norm(c.p40) < radius
  ensure norm(c.p41) < radius
  ensure norm(c.p42) < radius
  ensure norm(c.p43) < radius
  ensure norm(c.p44) < radius
  ensure norm(c.p45) < radius
  ensure norm(c.p46) < radius
  ensure norm(c.p47) < radius
  ensure norm(c.p48) < radius
  ensure norm(c.p49) < radius
}

forall Curve c where onSphere(c) {
  scalar radius = settings.sphereRadius
  ensure norm(c.p0) == radius
  ensure norm(c.p1) == radius
  ensure norm(c.p2) == radius
  ensure norm(c.p3) == radius
  ensure norm(c.p4) == radius
  ensure norm(c.p5) == radius
  ensure norm(c.p6) == radius
  ensure norm(c.p7) == radius
  ensure norm(c.p8) == radius
  ensure norm(c.p9) == radius
  ensure norm(c.p10) == radius
  ensure norm(c.p11) == radius
  ensure norm(c.p12) == radius
  ensure norm(c.p13) == radius
  ensure norm(c.p14) == radius
  ensure norm(c.p15) == radius
  ensure norm(c.p16) == radius
  ensure norm(c.p17) == radius
  ensure norm(c.p18) == radius
  ensure norm(c.p19) == radius
  ensure norm(c.p20) == radius
  ensure norm(c.p21) == radius
  ensure norm(c.p22) == radius
  ensure norm(c.p23) == radius
  ensure norm(c.p24) == radius
  ensure norm(c.p25) == radius
  ensure norm(c.p26) == radius
  ensure norm(c.p27) == radius
  ensure norm(c.p28) == radius
  ensure norm(c.p29) == radius
  ensure norm(c.p30) == radius
  ensure norm(c.p31) == radius
  ensure norm(c.p32) == radius
  ensure norm(c.p33) == radius
  ensure norm(c.p34) == radius
  ensure norm(c.p35) == radius
  ensure norm(c.p36) == radius
  ensure norm(c.p37) == radius
  ensure norm(c.p38) == radius
  ensure norm(c.p39) == radius
  ensure norm(c.p40) == radius
  ensure norm(c.p41) == radius
  ensure norm(c.p42) == radius
  ensure norm(c.p43) == radius
  ensure norm(c.p44) == radius
  ensure norm(c.p45) == radius
  ensure norm(c.p46) == radius
  ensure norm(c.p47) == radius
  ensure norm(c.p48) == radius
  ensure norm(c.p49) == radius

}

forall Curve c where hasShadow(c) {
  vec3 sq0 = ( c.p0[0] * cos(global.θ) + c.p0[2]*sin(global.θ), global.planeHeight, c.p0[2] * cos(global.θ) - c.p0[0] * sin(global.θ) )
  vec3 sq1 = ( c.p1[0] * cos(global.θ) + c.p1[2]*sin(global.θ), global.planeHeight, c.p1[2] * cos(global.θ) - c.p1[0] * sin(global.θ) )
  vec3 sq2 = ( c.p2[0] * cos(global.θ) + c.p2[2]*sin(global.θ), global.planeHeight, c.p2[2] * cos(global.θ) - c.p2[0] * sin(global.θ) )
  vec3 sq3 = ( c.p3[0] * cos(global.θ) + c.p3[2]*sin(global.θ), global.planeHeight, c.p3[2] * cos(global.θ) - c.p3[0] * sin(global.θ) )
  vec3 sq4 = ( c.p4[0] * cos(global.θ) + c.p4[2]*sin(global.θ), global.planeHeight, c.p4[2] * cos(global.θ) - c.p4[0] * sin(global.θ) )
  vec3 sq5 = ( c.p5[0] * cos(global.θ) + c.p5[2]*sin(global.θ), global.planeHeight, c.p5[2] * cos(global.θ) - c.p5[0] * sin(global.θ) )
  vec3 sq6 = ( c.p6[0] * cos(global.θ) + c.p6[2]*sin(global.θ), global.planeHeight, c.p6[2] * cos(global.θ) - c.p6[0] * sin(global.θ) )
  vec3 sq7 = ( c.p7[0] * cos(global.θ) + c.p7[2]*sin(global.θ), global.planeHeight, c.p7[2] * cos(global.θ) - c.p7[0] * sin(global.θ) )
  vec3 sq8 = ( c.p8[0] * cos(global.θ) + c.p8[2]*sin(global.θ), global.planeHeight, c.p8[2] * cos(global.θ) - c.p8[0] * sin(global.θ) )
  vec3 sq9 = ( c.p9[0] * cos(global.θ) + c.p9[2]*sin(global.θ), global.planeHeight, c.p9[2] * cos(global.θ) - c.p9[0] * sin(global.θ) )
  vec3 sq10 = ( c.p10[0] * cos(global.θ) + c.p10[2]*sin(global.θ), global.planeHeight, c.p10[2] * cos(global.θ) - c.p10[0] * sin(global.θ) )
  vec3 sq11 = ( c.p11[0] * cos(global.θ) + c.p11[2]*sin(global.θ), global.planeHeight, c.p11[2] * cos(global.θ) - c.p11[0] * sin(global.θ) )
  vec3 sq12 = ( c.p12[0] * cos(global.θ) + c.p12[2]*sin(global.θ), global.planeHeight, c.p12[2] * cos(global.θ) - c.p12[0] * sin(global.θ) )
  vec3 sq13 = ( c.p13[0] * cos(global.θ) + c.p13[2]*sin(global.θ), global.planeHeight, c.p13[2] * cos(global.θ) - c.p13[0] * sin(global.θ) )
  vec3 sq14 = ( c.p14[0] * cos(global.θ) + c.p14[2]*sin(global.θ), global.planeHeight, c.p14[2] * cos(global.θ) - c.p14[0] * sin(global.θ) )
  vec3 sq15 = ( c.p15[0] * cos(global.θ) + c.p15[2]*sin(global.θ), global.planeHeight, c.p15[2] * cos(global.θ) - c.p15[0] * sin(global.θ) )
  vec3 sq16 = ( c.p16[0] * cos(global.θ) + c.p16[2]*sin(global.θ), global.planeHeight, c.p16[2] * cos(global.θ) - c.p16[0] * sin(global.θ) )
  vec3 sq17 = ( c.p17[0] * cos(global.θ) + c.p17[2]*sin(global.θ), global.planeHeight, c.p17[2] * cos(global.θ) - c.p17[0] * sin(global.θ) )
  vec3 sq18 = ( c.p18[0] * cos(global.θ) + c.p18[2]*sin(global.θ), global.planeHeight, c.p18[2] * cos(global.θ) - c.p18[0] * sin(global.θ) )
  vec3 sq19 = ( c.p19[0] * cos(global.θ) + c.p19[2]*sin(global.θ), global.planeHeight, c.p19[2] * cos(global.θ) - c.p19[0] * sin(global.θ) )
  vec3 sq20 = ( c.p20[0] * cos(global.θ) + c.p20[2]*sin(global.θ), global.planeHeight, c.p20[2] * cos(global.θ) - c.p20[0] * sin(global.θ) )
  vec3 sq21 = ( c.p21[0] * cos(global.θ) + c.p21[2]*sin(global.θ), global.planeHeight, c.p21[2] * cos(global.θ) - c.p21[0] * sin(global.θ) )
  vec3 sq22 = ( c.p22[0] * cos(global.θ) + c.p22[2]*sin(global.θ), global.planeHeight, c.p22[2] * cos(global.θ) - c.p22[0] * sin(global.θ) )
  vec3 sq23 = ( c.p23[0] * cos(global.θ) + c.p23[2]*sin(global.θ), global.planeHeight, c.p23[2] * cos(global.θ) - c.p23[0] * sin(global.θ) )
  vec3 sq24 = ( c.p24[0] * cos(global.θ) + c.p24[2]*sin(global.θ), global.planeHeight, c.p24[2] * cos(global.θ) - c.p24[0] * sin(global.θ) )
  vec3 sq25 = ( c.p25[0] * cos(global.θ) + c.p25[2]*sin(global.θ), global.planeHeight, c.p25[2] * cos(global.θ) - c.p25[0] * sin(global.θ) )
  vec3 sq26 = ( c.p26[0] * cos(global.θ) + c.p26[2]*sin(global.θ), global.planeHeight, c.p26[2] * cos(global.θ) - c.p26[0] * sin(global.θ) )
  vec3 sq27 = ( c.p27[0] * cos(global.θ) + c.p27[2]*sin(global.θ), global.planeHeight, c.p27[2] * cos(global.θ) - c.p27[0] * sin(global.θ) )
  vec3 sq28 = ( c.p28[0] * cos(global.θ) + c.p28[2]*sin(global.θ), global.planeHeight, c.p28[2] * cos(global.θ) - c.p28[0] * sin(global.θ) )
  vec3 sq29 = ( c.p29[0] * cos(global.θ) + c.p29[2]*sin(global.θ), global.planeHeight, c.p29[2] * cos(global.θ) - c.p29[0] * sin(global.θ) )
  vec3 sq30 = ( c.p30[0] * cos(global.θ) + c.p30[2]*sin(global.θ), global.planeHeight, c.p30[2] * cos(global.θ) - c.p30[0] * sin(global.θ) )
  vec3 sq31 = ( c.p31[0] * cos(global.θ) + c.p31[2]*sin(global.θ), global.planeHeight, c.p31[2] * cos(global.θ) - c.p31[0] * sin(global.θ) )
  vec3 sq32 = ( c.p32[0] * cos(global.θ) + c.p32[2]*sin(global.θ), global.planeHeight, c.p32[2] * cos(global.θ) - c.p32[0] * sin(global.θ) )
  vec3 sq33 = ( c.p33[0] * cos(global.θ) + c.p33[2]*sin(global.θ), global.planeHeight, c.p33[2] * cos(global.θ) - c.p33[0] * sin(global.θ) )
  vec3 sq34 = ( c.p34[0] * cos(global.θ) + c.p34[2]*sin(global.θ), global.planeHeight, c.p34[2] * cos(global.θ) - c.p34[0] * sin(global.θ) )
  vec3 sq35 = ( c.p35[0] * cos(global.θ) + c.p35[2]*sin(global.θ), global.planeHeight, c.p35[2] * cos(global.θ) - c.p35[0] * sin(global.θ) )
  vec3 sq36 = ( c.p36[0] * cos(global.θ) + c.p36[2]*sin(global.θ), global.planeHeight, c.p36[2] * cos(global.θ) - c.p36[0] * sin(global.θ) )
  vec3 sq37 = ( c.p37[0] * cos(global.θ) + c.p37[2]*sin(global.θ), global.planeHeight, c.p37[2] * cos(global.θ) - c.p37[0] * sin(global.θ) )
  vec3 sq38 = ( c.p38[0] * cos(global.θ) + c.p38[2]*sin(global.θ), global.planeHeight, c.p38[2] * cos(global.θ) - c.p38[0] * sin(global.θ) )
  vec3 sq39 = ( c.p39[0] * cos(global.θ) + c.p39[2]*sin(global.θ), global.planeHeight, c.p39[2] * cos(global.θ) - c.p39[0] * sin(global.θ) )
  vec3 sq40 = ( c.p40[0] * cos(global.θ) + c.p40[2]*sin(global.θ), global.planeHeight, c.p40[2] * cos(global.θ) - c.p40[0] * sin(global.θ) )
  vec3 sq41 = ( c.p41[0] * cos(global.θ) + c.p41[2]*sin(global.θ), global.planeHeight, c.p41[2] * cos(global.θ) - c.p41[0] * sin(global.θ) )
  vec3 sq42 = ( c.p42[0] * cos(global.θ) + c.p42[2]*sin(global.θ), global.planeHeight, c.p42[2] * cos(global.θ) - c.p42[0] * sin(global.θ) )
  vec3 sq43 = ( c.p43[0] * cos(global.θ) + c.p43[2]*sin(global.θ), global.planeHeight, c.p43[2] * cos(global.θ) - c.p43[0] * sin(global.θ) )
  vec3 sq44 = ( c.p44[0] * cos(global.θ) + c.p44[2]*sin(global.θ), global.planeHeight, c.p44[2] * cos(global.θ) - c.p44[0] * sin(global.θ) )
  vec3 sq45 = ( c.p45[0] * cos(global.θ) + c.p45[2]*sin(global.θ), global.planeHeight, c.p45[2] * cos(global.θ) - c.p45[0] * sin(global.θ) )
  vec3 sq46 = ( c.p46[0] * cos(global.θ) + c.p46[2]*sin(global.θ), global.planeHeight, c.p46[2] * cos(global.θ) - c.p46[0] * sin(global.θ) )
  vec3 sq47 = ( c.p47[0] * cos(global.θ) + c.p47[2]*sin(global.θ), global.planeHeight, c.p47[2] * cos(global.θ) - c.p47[0] * sin(global.θ) )
  vec3 sq48 = ( c.p48[0] * cos(global.θ) + c.p48[2]*sin(global.θ), global.planeHeight, c.p48[2] * cos(global.θ) - c.p48[0] * sin(global.θ) )
  vec3 sq49 = ( c.p49[0] * cos(global.θ) + c.p49[2]*sin(global.θ), global.planeHeight, c.p49[2] * cos(global.θ) - c.p49[0] * sin(global.θ) )

  vec2 sr0 = canvas.width * (sq0[0], sq0[1]) / (sq0[2] - global.cZ)
  vec2 sr1 = canvas.width * (sq1[0], sq1[1]) / (sq1[2] - global.cZ)
  vec2 sr2 = canvas.width * (sq2[0], sq2[1]) / (sq2[2] - global.cZ)
  vec2 sr3 = canvas.width * (sq3[0], sq3[1]) / (sq3[2] - global.cZ)
  vec2 sr4 = canvas.width * (sq4[0], sq4[1]) / (sq4[2] - global.cZ)
  vec2 sr5 = canvas.width * (sq5[0], sq5[1]) / (sq5[2] - global.cZ)
  vec2 sr6 = canvas.width * (sq6[0], sq6[1]) / (sq6[2] - global.cZ)
  vec2 sr7 = canvas.width * (sq7[0], sq7[1]) / (sq7[2] - global.cZ)
  vec2 sr8 = canvas.width * (sq8[0], sq8[1]) / (sq8[2] - global.cZ)
  vec2 sr9 = canvas.width * (sq9[0], sq9[1]) / (sq9[2] - global.cZ)
  vec2 sr10 = canvas.width * (sq10[0], sq10[1]) / (sq10[2] - global.cZ)
  vec2 sr11 = canvas.width * (sq11[0], sq11[1]) / (sq11[2] - global.cZ)
  vec2 sr12 = canvas.width * (sq12[0], sq12[1]) / (sq12[2] - global.cZ)
  vec2 sr13 = canvas.width * (sq13[0], sq13[1]) / (sq13[2] - global.cZ)
  vec2 sr14 = canvas.width * (sq14[0], sq14[1]) / (sq14[2] - global.cZ)
  vec2 sr15 = canvas.width * (sq15[0], sq15[1]) / (sq15[2] - global.cZ)
  vec2 sr16 = canvas.width * (sq16[0], sq16[1]) / (sq16[2] - global.cZ)
  vec2 sr17 = canvas.width * (sq17[0], sq17[1]) / (sq17[2] - global.cZ)
  vec2 sr18 = canvas.width * (sq18[0], sq18[1]) / (sq18[2] - global.cZ)
  vec2 sr19 = canvas.width * (sq19[0], sq19[1]) / (sq19[2] - global.cZ)
  vec2 sr20 = canvas.width * (sq20[0], sq20[1]) / (sq20[2] - global.cZ)
  vec2 sr21 = canvas.width * (sq21[0], sq21[1]) / (sq21[2] - global.cZ)
  vec2 sr22 = canvas.width * (sq22[0], sq22[1]) / (sq22[2] - global.cZ)
  vec2 sr23 = canvas.width * (sq23[0], sq23[1]) / (sq23[2] - global.cZ)
  vec2 sr24 = canvas.width * (sq24[0], sq24[1]) / (sq24[2] - global.cZ)
  vec2 sr25 = canvas.width * (sq25[0], sq25[1]) / (sq25[2] - global.cZ)
  vec2 sr26 = canvas.width * (sq26[0], sq26[1]) / (sq26[2] - global.cZ)
  vec2 sr27 = canvas.width * (sq27[0], sq27[1]) / (sq27[2] - global.cZ)
  vec2 sr28 = canvas.width * (sq28[0], sq28[1]) / (sq28[2] - global.cZ)
  vec2 sr29 = canvas.width * (sq29[0], sq29[1]) / (sq29[2] - global.cZ)
  vec2 sr30 = canvas.width * (sq30[0], sq30[1]) / (sq30[2] - global.cZ)
  vec2 sr31 = canvas.width * (sq31[0], sq31[1]) / (sq31[2] - global.cZ)
  vec2 sr32 = canvas.width * (sq32[0], sq32[1]) / (sq32[2] - global.cZ)
  vec2 sr33 = canvas.width * (sq33[0], sq33[1]) / (sq33[2] - global.cZ)
  vec2 sr34 = canvas.width * (sq34[0], sq34[1]) / (sq34[2] - global.cZ)
  vec2 sr35 = canvas.width * (sq35[0], sq35[1]) / (sq35[2] - global.cZ)
  vec2 sr36 = canvas.width * (sq36[0], sq36[1]) / (sq36[2] - global.cZ)
  vec2 sr37 = canvas.width * (sq37[0], sq37[1]) / (sq37[2] - global.cZ)
  vec2 sr38 = canvas.width * (sq38[0], sq38[1]) / (sq38[2] - global.cZ)
  vec2 sr39 = canvas.width * (sq39[0], sq39[1]) / (sq39[2] - global.cZ)
  vec2 sr40 = canvas.width * (sq40[0], sq40[1]) / (sq40[2] - global.cZ)
  vec2 sr41 = canvas.width * (sq41[0], sq41[1]) / (sq41[2] - global.cZ)
  vec2 sr42 = canvas.width * (sq42[0], sq42[1]) / (sq42[2] - global.cZ)
  vec2 sr43 = canvas.width * (sq43[0], sq43[1]) / (sq43[2] - global.cZ)
  vec2 sr44 = canvas.width * (sq44[0], sq44[1]) / (sq44[2] - global.cZ)
  vec2 sr45 = canvas.width * (sq45[0], sq45[1]) / (sq45[2] - global.cZ)
  vec2 sr46 = canvas.width * (sq46[0], sq46[1]) / (sq46[2] - global.cZ)
  vec2 sr47 = canvas.width * (sq47[0], sq47[1]) / (sq47[2] - global.cZ)
  vec2 sr48 = canvas.width * (sq48[0], sq48[1]) / (sq48[2] - global.cZ)
  vec2 sr49 = canvas.width * (sq49[0], sq49[1]) / (sq49[2] - global.cZ)

  a = 0.2
  b = -0.001

  shape c.line0 = Line{
	start: (sr0[0], sr0[1])
	end: (sr1[0], sr1[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq0[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line1 = Line{
	start: (sr1[0], sr1[1])
	end: (sr2[0], sr2[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq1[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line2 = Line{
	start: (sr2[0], sr2[1])
	end: (sr3[0], sr3[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq2[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line3 = Line{
	start: (sr3[0], sr3[1])
	end: (sr4[0], sr4[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq3[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line4 = Line{
	start: (sr4[0], sr4[1])
	end: (sr5[0], sr5[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq4[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line5 = Line{
	start: (sr5[0], sr5[1])
	end: (sr6[0], sr6[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq5[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line6 = Line{
	start: (sr6[0], sr6[1])
	end: (sr7[0], sr7[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq6[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line7 = Line{
	start: (sr7[0], sr7[1])
	end: (sr8[0], sr8[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq7[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line8 = Line{
	start: (sr8[0], sr8[1])
	end: (sr9[0], sr9[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq8[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line9 = Line{
	start: (sr9[0], sr9[1])
	end: (sr10[0], sr10[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq9[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line10 = Line{
	start: (sr10[0], sr10[1])
	end: (sr11[0], sr11[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq10[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line11 = Line{
	start: (sr11[0], sr11[1])
	end: (sr12[0], sr12[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq11[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line12 = Line{
	start: (sr12[0], sr12[1])
	end: (sr13[0], sr13[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq12[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line13 = Line{
	start: (sr13[0], sr13[1])
	end: (sr14[0], sr14[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq13[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line14 = Line{
	start: (sr14[0], sr14[1])
	end: (sr15[0], sr15[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq14[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line15 = Line{
	start: (sr15[0], sr15[1])
	end: (sr16[0], sr16[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq15[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line16 = Line{
	start: (sr16[0], sr16[1])
	end: (sr17[0], sr17[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq16[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line17 = Line{
	start: (sr17[0], sr17[1])
	end: (sr18[0], sr18[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq17[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line18 = Line{
	start: (sr18[0], sr18[1])
	end: (sr19[0], sr19[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq18[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line19 = Line{
	start: (sr19[0], sr19[1])
	end: (sr20[0], sr20[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq19[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line20 = Line{
	start: (sr20[0], sr20[1])
	end: (sr21[0], sr21[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq20[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line21 = Line{
	start: (sr21[0], sr21[1])
	end: (sr22[0], sr22[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq21[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line22 = Line{
	start: (sr22[0], sr22[1])
	end: (sr23[0], sr23[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq22[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line23 = Line{
	start: (sr23[0], sr23[1])
	end: (sr24[0], sr24[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq23[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line24 = Line{
	start: (sr24[0], sr24[1])
	end: (sr25[0], sr25[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq24[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line25 = Line{
	start: (sr25[0], sr25[1])
	end: (sr26[0], sr26[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq25[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line26 = Line{
	start: (sr26[0], sr26[1])
	end: (sr27[0], sr27[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq26[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line27 = Line{
	start: (sr27[0], sr27[1])
	end: (sr28[0], sr28[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq27[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line28 = Line{
	start: (sr28[0], sr28[1])
	end: (sr29[0], sr29[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq28[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line29 = Line{
	start: (sr29[0], sr29[1])
	end: (sr30[0], sr30[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq29[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line30 = Line{
	start: (sr30[0], sr30[1])
	end: (sr31[0], sr31[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq30[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line31 = Line{
	start: (sr31[0], sr31[1])
	end: (sr32[0], sr32[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq31[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line32 = Line{
	start: (sr32[0], sr32[1])
	end: (sr33[0], sr33[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq32[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line33 = Line{
	start: (sr33[0], sr33[1])
	end: (sr34[0], sr34[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq33[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line34 = Line{
	start: (sr34[0], sr34[1])
	end: (sr35[0], sr35[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq34[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line35 = Line{
	start: (sr35[0], sr35[1])
	end: (sr36[0], sr36[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq35[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line36 = Line{
	start: (sr36[0], sr36[1])
	end: (sr37[0], sr37[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq36[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line37 = Line{
	start: (sr37[0], sr37[1])
	end: (sr38[0], sr38[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq37[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line38 = Line{
	start: (sr38[0], sr38[1])
	end: (sr39[0], sr39[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq38[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line39 = Line{
	start: (sr39[0], sr39[1])
	end: (sr40[0], sr40[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq39[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line40 = Line{
	start: (sr40[0], sr40[1])
	end: (sr41[0], sr41[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq40[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line41 = Line{
	start: (sr41[0], sr41[1])
	end: (sr42[0], sr42[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq41[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line42 = Line{
	start: (sr42[0], sr42[1])
	end: (sr43[0], sr43[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq42[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line43 = Line{
	start: (sr43[0], sr43[1])
	end: (sr44[0], sr44[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq43[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line44 = Line{
	start: (sr44[0], sr44[1])
	end: (sr45[0], sr45[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq44[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line45 = Line{
	start: (sr45[0], sr45[1])
	end: (sr46[0], sr46[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq45[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line46 = Line{
	start: (sr46[0], sr46[1])
	end: (sr47[0], sr47[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq46[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line47 = Line{
	start: (sr47[0], sr47[1])
	end: (sr48[0], sr48[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq47[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line48 = Line{
	start: (sr48[0], sr48[1])
	end: (sr49[0], sr49[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq48[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }
  shape c.line49 = Line{
	start: (sr49[0], sr49[1])
	end: (sr0[0], sr0[1])
	strokeColor: rgba(0, 0, 0, min(1.0, max(0.0, a + b * sq49[2])))
	strokeWidth: settings.lineThickness
	ensureOnCanvas: false
  }

}

forall Curve c1; Curve c2 {
  cm1 = centerOfMass(c1.points2d)
  cm2 = centerOfMass(c2.points2d)
  encourage 200 < vdist(cm1, cm2)
}
`,l=`type Curve

predicate onSphere(Curve)
predicate inSphere(Curve)
predicate hasShadow(Curve)
`,t={substance:s,style:[{contents:a,resolver:e}],domain:l,variation:"SkywardLobster2569"};export{t as default};
//# sourceMappingURL=space-curves.trio-6eda4394.js.map
