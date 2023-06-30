import{a as e,i as n,j as t,k as o,l as r}from"./index-d0dd7cba.js";const a=e("geometry-domain/textbook_problems"),s=`global {
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
}`,l={substance:n,style:[{contents:t,resolver:o},{contents:s,resolver:a}],domain:r,variation:"RationalityZebra567",excludeWarnings:["BBoxApproximationWarning"]};export{l as default};
