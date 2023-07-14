import{s as e,r as n}from"./euclidean.style-a8c3bd01.js";import{m as o}from"./resolver-84c3bc8f.js";import{d as r}from"./geometry.domain-952a3e66.js";import"./iframe-38af56f5.js";const t=`-- TODO full example can sometimes throw error "2147482504"
Point A, B, C, D, E, F, d, b
Circle c := CircleR(F, C)
Segment AC := Chord(c, A, C)
Segment EC := Chord(c, E, C)
Segment FB := Radius(c, B)
Segment FD := Radius(c, D)
Segment FC := Radius(c, C)
Segment Fd := PerpendicularBisectorLabelPts(EC, F, d)
Segment Fb := PerpendicularBisectorLabelPts(AC, F, b)
On(b, FB)
On(d, FD)
Collinear(F, b, B)
Collinear(F, d, D)
Segment EA := Diameter(c, E, A)
AutoLabel A, B, C, D, E, F, d, b`,l=o("geometry-domain/textbook_problems"),i=`global {
    shape background = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: #F6F4F2
    } 
}

forall Point p {
  override p.color = #425664
  override p.icon.fillColor = #425664
  override p.icon.strokeColor = #425664
  override p.text.fillColor = #425664
}

forall Linelike l {
  override l.color = #425664

  override l.icon.strokeColor = #425664
}

forall Circle c {
  override c.icon.strokeColor = #425664
}

forall Segment s
where s := PerpendicularBisectorLabelPts(s2, p1, p2)
with Segment s2; Point p1, p2 {
  override s.icon.strokeColor = #425664
  override s.mark.strokeColor = #425664
}

`,m={substance:t,style:[{contents:e,resolver:n},{contents:i,resolver:l}],domain:r,variation:"EdwardGuanaco94367",excludeWarnings:["NoopDeleteWarning"]};export{m as default};
//# sourceMappingURL=c11p12.trio-394d42b9.js.map
