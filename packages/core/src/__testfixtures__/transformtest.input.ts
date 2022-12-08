export const objDict = {
    // autodiff
    testpow1: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
        Math.pow(x - 7, 3 + 2),

    //autodiff
    testpow2: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        ops.vdistsq(Math.pow(x - 3, 22)),

    //autodiff
    testpow3: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        x + Math.pow(4, y - z),
    
    //autodiff
    testpow4: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    x + Math.pow(4, Math.pow(3, 8)),

    //autodiff
    testpow5: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    ops.vdistsq(2 + (x ? 3 : bob(Math.pow(4, 4)))),

    //autodiff
    testbops1: ([t1, s1]: [string, any]) => x + 3,

    //autodiff
    testbops2: (x: number) => x + 2 - 3 + 5 * 7 / 2 - (3 + 4),

    //autodiff
    testbops3: (x: number) => x + 3 + (2 + (1 + 4)),

    // autodiff
    testsq1: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
    Math.pow(x - 7, 2),

    //autodiff
    testsq2: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        ops.vdistsq(Math.pow(x - 3, 2)),

    //autodiff
    testsq3: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
        x + Math.pow(4, 2),
    
    //autodiff
    testsq4: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    x + Math.pow(Math.pow(3, 2), 2),

    //autodiff
    testsq5: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => 
    ops.vdistsq(2 + (x ? 3 : bob(Math.pow(4, 2)))),

    //autodiff
    testtyps1: (x : number, y: number) : number => 3,

    //autodiff
    testuops1: (x: number) => -x,

    //autodiff
    testuops2: (x: number) => -ops.vdistsq(3, 2),

    //autodiff
    testuops3: (x: number) => -7,

    //autodiff
    testuops4: (x: number) => -(3 + -5 * (2 ? x : y)),

    //autodiff
    testtern1: (x: any) => x ? 3 : 4-2,

    //autodiff
    testtern2: (x: any) => bob.sue(x + (Math.pow(x, 2) ? ahd(z) : 2)),

    //autodiff
    testtern3: (x: any) => Math.pow(x, z ? 4 : 2),

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