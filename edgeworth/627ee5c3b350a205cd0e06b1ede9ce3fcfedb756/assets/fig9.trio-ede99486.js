import{f as r,g as n,h as o}from"./index-0672ecd7.js";const e=`Vertex main
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
`,s={substance:e,style:[{contents:r,resolver:n}],domain:o,variation:"TurquishMongoose642"};export{s as default};
