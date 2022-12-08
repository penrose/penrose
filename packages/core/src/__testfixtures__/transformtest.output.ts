export const objDict = {
    // autodiff
    testpow1: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
        pow(sub(x, 7), add(3, 2)),

    //autodiff
    testpow2: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        ops.vdistsq(pow(sub(x, 3), 22)),

    //autodiff
    testpow3: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        add(x, pow(4, sub(y, z))),
    
    //autodiff
    testpow4: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    add(x, pow(4, pow(3, 8))),

    //autodiff
    testpow5: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    ops.vdistsq(add(2, (ifCond(x, 3, bob(pow(4, 4)))))),

    //autodiff
    testbops1: ([t1, s1]: [string, any]) => add(x, 3),

    //autodiff
    testbops2: (x: ad.Num) => sub(add(sub(add(x, 2), 3), div(mul(5, 7), 2)), (add(3, 4))),

    //autodiff
    testbops3: (x: ad.Num) => add(add(x, 3), (add(2, (add(1, 4))))),

    // autodiff
    testsq1: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
    squared(sub(x, 7)),

    //autodiff
    testsq2: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        ops.vdistsq(squared(sub(x, 3))),

    //autodiff
    testsq3: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        add(x, squared(4)),
    
    //autodiff
    testsq4: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    add(x, squared(squared(3))),

    //autodiff
    testsq5: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    ops.vdistsq(add(2, (ifCond(x, 3, bob(squared(4)))))),

    //autodiff
    testtyps1: (x : ad.Num, y: ad.Num) : ad.Num => 3,

    //autodiff
    testuops1: (x: ad.Num) => neg(x),

    //autodiff
    testuops2: (x: ad.Num) => neg(ops.vdistsq(3, 2)),

    //autodiff
    testuops3: (x: ad.Num) => -7,

    //autodiff
    testuops4: (x: ad.Num) => neg((add(3, mul(-5, (ifCond(2, x, y)))))),

    //autodiff
    testtern1: (x: any) => ifCond(x, 3, sub(4, 2)),

    //autodiff
    testtern2: (x: any) => bob.sue(add(x, (ifCond(squared(x), ahd(z), 2)))),

    //autodiff
    testtern3: (x: any) => pow(x, ifCond(z, 4, 2)),

    // //autodiff       -- these can only be tested when the content of MEMEXP and IDENT are uncommented in toCustomAD.ts, which is ok only for testing
    // see testkey for the answers to these
    // testid1: (x: any) => norm + 2,

    // //autodiff
    // testid2: (norm: any) => abs(3, -(ops.vdistsq(norm) + 7))
 
    // //autodiff
    // testmemexp1: (x: any) => a.b,

    // //autodiff
    // testmemexp2: (x: any) => a.b(g, e, h, a.b(bob)),

    // //autodiff
    // testmemexp3: (x: any) => bob(2 + Math.pow(a.b, 2))
  }