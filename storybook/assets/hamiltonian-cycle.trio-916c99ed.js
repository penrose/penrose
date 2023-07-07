import{s as e,r,d as n}from"./simple-directed-graph.domain-d3bc01bc.js";import{m as t}from"./resolver-2719b1fe.js";import"./iframe-4581b948.js";const o=`-- The seven vertices are a, b, c, d, e, f, g
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

AutoLabel All`,a=t("graph-domain/other-examples"),c=`forall Vertex v where HighlightVertex(v) {
    override v.dot.fillColor = color.redOrange
    override v.text.fillColor = color.redOrange
}

forall Vertex a, b where Arc(a,b) as e; HighlightArc(a,b) {
    override e.arrow.strokeColor = color.redOrange 
    override e.pointer.fillColor = color.redOrange
}`,h={substance:o,style:[{contents:e,resolver:r},{contents:c,resolver:a}],domain:n,variation:"PredictableWasp290",excludeWarnings:[]};export{h as default};
//# sourceMappingURL=hamiltonian-cycle.trio-916c99ed.js.map
