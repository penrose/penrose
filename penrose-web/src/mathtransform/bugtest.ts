export const objDict = {
  // autodiff
    testpow3: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
      ops.vdistsq(Math.pow(x - 3, 2)),
}