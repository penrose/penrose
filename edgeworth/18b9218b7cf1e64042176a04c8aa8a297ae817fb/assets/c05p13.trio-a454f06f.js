import{a as e,i as t,j as n,k as o,l as r}from"./index-6d8b3b93.js";const a=e("geometry-domain/textbook_problems"),s=`global {
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
}`,i={substance:t,style:[{contents:n,resolver:o},{contents:s,resolver:a}],domain:r,variation:"RationalityZebra567"};export{i as default};
