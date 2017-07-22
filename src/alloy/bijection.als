sig A { f : B }
sig B {  }

fact { 
    all a1,a2: A | a1.f = a2.f implies a1 = a2   // r is injective
    all b: B | some a:A | a.f = b   // r in surjective
}

pred show() {}

run show for 5
