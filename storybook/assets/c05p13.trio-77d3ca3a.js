import{s as n,r as e}from"./euclidean.style-1d7afe8d.js";import{m as t}from"./resolver-f1d01b85.js";import{d as r}from"./geometry.domain-952a3e66.js";import"./iframe-02305a86.js";const o=`-- name in registry: incenter-triangle
Point J, K, L, P, m
Let JKL := Triangle(J, K, L)
Incenter(P, JKL)
-- Centroid(P, JKL)
-- Circumcenter(P, JKL)
-- Orthocenter(P, JKL)
Let KL := Segment(K, L)
Collinear(K, m, L)
Let PLM := Triangle(P, L, m)
Angle PML := InteriorAngle(P, m, L)
RightMarked(PML)
AutoLabel J, K, L, P, m
`,i=t("geometry-domain/textbook_problems"),a=`global {
    shape background = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: #DDEDF4
    } 
}

forall Point p
with Triangle T; Point t1, t2, t3
where T := Triangle(t1, t2, t3); Incenter(p, T) {
  override T.incenterIcon.strokeColor = setOpacity(Colors.darkpurple, 0.8)
}`,L={substance:o,style:[{contents:n,resolver:e},{contents:a,resolver:i}],domain:r,variation:"RationalityZebra567",excludeWarnings:["BBoxApproximationWarning"]};export{L as default};
//# sourceMappingURL=c05p13.trio-77d3ca3a.js.map
