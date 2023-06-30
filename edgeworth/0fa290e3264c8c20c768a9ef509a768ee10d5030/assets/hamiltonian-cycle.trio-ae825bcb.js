import{a as e,f as r,g as n,h as t}from"./index-82fd0888.js";const a=`-- The seven vertices are a, b, c, d, e, f, g
Vertex a, b, c, d, e, f, g

-- The Hamiltonian cycle is a -> b -> c -> d -> e -> f -> g -> a
Arc(a, b)
Arc(b, c)
Arc(c, d)
Arc(d, e)
Arc(e, f)
Arc(f, g)
Arc(g, a)

-- Highlight the start/end vertex and arcs in the cycle
HighlightVertex(a)

HighlightArc(a, b)
HighlightArc(b, c)
HighlightArc(c, d)
HighlightArc(d, e)
HighlightArc(e, f)
HighlightArc(f, g)
HighlightArc(g, a)

-- Additional edges that are not in the Hamiltonian cycle
Arc(a, c)
Arc(a, d)
Arc(b, e)
Arc(b, f)
Arc(c, g)
Arc(d, f)

AutoLabel All`,c=e("graph-domain/other-examples"),l=`forall Vertex v where HighlightVertex(v) {
    override v.dot.fillColor = color.redOrange
    override v.text.fillColor = color.redOrange
}

forall Vertex a, b where Arc(a,b) as e; HighlightArc(a,b) {
    override e.arrow.strokeColor = color.redOrange 
    override e.pointer.fillColor = color.redOrange
}`,i={substance:a,style:[{contents:r,resolver:n},{contents:l,resolver:c}],domain:t,variation:"PredictableWasp290"};export{i as default};
