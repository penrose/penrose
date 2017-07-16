sig A { f : one B } // f is a FUNCTION from A to B
sig B {  }

fact {
    // all a1,a2: A | a1.f = a2.f implies a1 = a2   // r is injective
    // all b: B | some a:A | a.f = b   // r in surjective
    some f
    all a: A | some b1,b2 : B | b1 != b2 and a.f = b1 and a.f = b2
}

pred show() {}

run show for 5
