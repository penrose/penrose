import{m as n}from"./resolver-84c3bc8f.js";import"./iframe-38af56f5.js";const e=`Center r

Circle c1, c2, c3

Shape ic1 := Inversion(c1, r)
Shape ic2 := Inversion(c2, r)
Shape ic3 := Inversion(c3, r)

Label r $S^1$

Label c1 $c_1$
Label c2 $c_2$
Label c3 $c_3$

Label ic1 $c'_1$
Label ic2 $c'_2$
Label ic3 $c'_3$`,r=n("mobius"),c=`canvas {
    width = 800
    height = 700
}

global {
   shape background = Image {
      href: "background.svg"
      center: (0,0)
      width: canvas.width
      height: canvas.height
      opacity: .6
      preserveAspectRatio: "none"
   }
}

forall Shape s
where s has label
{
    shape s.eq = Equation {
        center: s.center
        string: s.label
        fontSize: "25px"
    }
}

forall Center c {
    vec2 c.center = (?, ?)
    scalar c.r = ?
    shape c.circle = Circle {
        center: c.center
        r: c.r
        fillColor: rgba(0, 0, 0, 0)
        strokeWidth: 2
        strokeStyle: "dashed"
        strokeColor: rgba(0, 0, 0, 1)
    }
    shape c.point = Circle {
        center: c.center
        fillColor: rgba(0, 0, 0, 1)
        r: 3
    }
    ensure lessThan(100, c.r)
}

forall Center c
where c has label
{
    shape c.eq = Equation {
        string: c.label
        fontSize: "25px"
    }
    ensure touching(c.eq, c.circle, 5)
}

forall Circle c {
    vec2 c.center = (?, ?)
    scalar c.r = ?
    shape c.circle = Circle {
        center: c.center
        r: c.r
    }
    ensure lessThan(30, c.r)

    shape c.shading = Image {
        href: "sphere-shading.svg"
        center: c.center
        width: 2*c.r
        height: 2*c.r
        ensureOnCanvas: false
        opacity: .5
    }
    layer c.circle above c.shading
}

forall Point p {
    vec2 p.pos = (?, ?)
    shape p.point = Circle {
      r: 5
      center: p.pos
    }
}

forall Shape s; Point p; Center r
where s := Inversion( p, r ) {
    scalar s.d2 = vdistsq(p.pos, r.center)
    scalar s.coef = r.r * r.r / s.d2
    vec2 s.pos = r.center + s.coef * (p.pos - r.center)
    shape s.point = Circle {
        r: 5
        fillColor: p.point.fillColor
        center: s.pos
    }
}

forall Shape s; Circle c; Center r
where s := Inversion( c, r ) {
    scalar s.d = vdist( c.center, r.center )

    scalar s.coef1 = ( s.d - c.r ) / s.d
    vec2 s.p1 = r.center + s.coef1 * (c.center - r.center)

    scalar s.i1coef = r.r * r.r / vdistsq( r.center, s.p1 )
    vec2 s.ip1 = r.center + s.i1coef * ( s.p1  - r.center)

    scalar s.coef2 = (s.d + c.r) / s.d
    vec2 s.p2 = r.center + s.coef2 * (c.center - r.center)

    scalar s.i2coef = r.r * r.r / vdistsq( r.center, s.p2 )
    vec2 s.ip2 = r.center + s.i2coef * ( s.p2  - r.center )

    scalar s.r = vdist(s.ip1, s.ip2) / 2
    vec2 s.center = 0.5 * (s.ip1 + s.ip2)

    shape s.circle = Circle {
        r: s.r
        fillColor: c.circle.fillColor
        center: s.center
    }

    shape s.shading = Image {
        href: "sphere-shading.svg"
        center: s.circle.center
        width: 2*s.circle.r
        height: 2*s.circle.r
        ensureOnCanvas: false
        opacity: .5
    }
    layer s.circle above s.shading

    ensure lessThan(30, s.r)
}

forall Circle c1; Circle c2 {
    ensure touching(c1.circle, c2.circle)
}

forall Center r; Circle c {
    ensure contains(r.circle, c.circle, 30)
}

forall Shape s1; Shape s2 {
    s1.eq above s2.circle
    s1.eq above s1.circle
    s2.eq above s1.circle
    s2.eq above s2.circle
}

`,s=`type Center
type Shape

type Point
type Circle

Point <: Shape
Circle <: Shape

constructor Inversion( Shape original, Center center ) -> Shape`,l={substance:e,style:[{contents:c,resolver:r}],domain:s,variation:"HibernationPony6425",excludeWarnings:[]};export{l as default};
//# sourceMappingURL=mobius.trio-f9ef58bd.js.map
