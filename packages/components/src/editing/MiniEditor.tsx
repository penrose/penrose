import { compileDomain } from "@penrose/core";
import mesh2d from "@penrose/examples/dist/triangle-mesh-2d";
import { useState } from "react";
import { Simple } from "../Simple";
import EditorPane from "./EditorPane";

interface StyledSubstance {
  substance: string;
  inlineStyle: string;
}

const concyclicStyle = `
-- make vertex dots bigger
forall Vertex v {
   override v.icon.r = 1.75*Global.vertexRadius
}

-- make corner labels larger and dark blue
forall Corner c; Vertex i; Vertex j; Vertex k
where c := MakeCorner(i,j,k); c has text label {
   override c.text.fontSize = "25px"
   override c.text.fillColor = Colors.darkBlue
}

-- don't draw black outline on arcs
forall Corner c; Vertex i; Vertex j; Vertex k
where c := MakeCorner(i,j,k) {
   delete c.arc
}

-- make edges thicker
forall Edge e {
   override e.segment.strokeWidth = 2*Global.lineThickness
}

-- make boundary edges thicker
forall Edge e
where IsBoundaryEdge( e ) {
   override e.segment.strokeWidth = 3*Global.lineThickness
}

-- corner labels shouldn't overlap diagonals
forall Edge e; Corner c; Vertex i; Vertex j; Vertex k; Vertex l
where c := MakeCorner(i,j,k); e := MakeEdge(l,i) {
   ensure disjoint( c.textBounds, e.segment )
}
forall Edge e; Corner c; Vertex i; Vertex j; Vertex k; Vertex l
where c := MakeCorner(i,j,k); e := MakeEdge(i,l) {
   ensure disjoint( c.textBounds, e.segment )
}
`;

const builtins: StyledSubstance[] = [
  {
    substance: mesh2d.diagrams["concyclic-pair.substance"],
    inlineStyle: concyclicStyle,
  },
];

interface MiniEditorProps {
  style: string;
  domain: string;
}

export default function ({ style, domain }: MiniEditorProps) {
  const [substance, setSubstance] = useState(builtins[0].substance);
  const [inlineStyle, setInlineStyle] = useState(builtins[0].inlineStyle);
  const combinedStyle = style + inlineStyle;
  const domainCache = compileDomain(domain).unsafelyUnwrap();
  return (
    <div
      style={{
        display: "flex",
        height: "500px",
      }}
    >
      <EditorPane
        domainCache={domainCache}
        value={substance}
        vimMode={true}
        languageType={"substance"}
        onWrite={() => {}}
        onChange={(p: string) => setSubstance(p)}
      />
      <EditorPane
        domainCache={domainCache}
        value={inlineStyle}
        vimMode={true}
        languageType={"style"}
        onWrite={() => {}}
        onChange={(p: string) => setInlineStyle(p)}
      />
      <Simple
        substance={substance}
        style={combinedStyle}
        domain={domain}
        variation={"const"}
      ></Simple>
    </div>
  );
}
