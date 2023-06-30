import{f as r,g as n,h as e}from"./index-d0dd7cba.js";const o=`Vertex main
Vertex display, parser, protocol
Vertex AST, page, network

Arc(main, display)
Arc(main, parser)
Arc(main, protocol)
Arc(main, AST)
Arc(display, AST)
Arc(parser, AST)
Arc(parser, page)
Arc(protocol, page)
Arc(protocol, network)

AutoLabel All
`,s={substance:o,style:[{contents:r,resolver:n}],domain:e,variation:"TurquishMongoose642",excludeWarnings:[]};export{s as default};
