import{m as e}from"./resolver-691c4a84.js";import"./iframe-f4259546.js";const n=`AWord Shells
AWord Minkowski
AWord Exact
AWord Riemannian
AWord BCs
AWord CurveSurface

BWord Tools
BWord Survey
BWord Courses
BWord Software
BWord Videos

AutoLabel All
Label Riemannian "shape space formulation"
Label BCs "boundary conditions"
Label Shells "repulsive shells"
Label Exact "exact discretization"
Label CurveSurface "repulsive curves + surfaces"
Label Minkowski "Minkowski penalties"
Label Tools "end-user tools"
Label Survey "survey article"
Label Courses "local & outreach courses"
Label Software "open-source software"
Label Videos "K-12 videos"

`,o=e("word-cloud"),r=`-- Set the target diagram size
canvas {
   width = 240
   height = 180
}

-- A few colors used throughout
Colors {
   color black = rgba(0.,0.,0.,1.)
   color lightBlue = rgba( 27./255., 31./255., 138./255., .2 )
   color lightGreen = rgba( 27./255., 138./255., 31./255., .2 )
}

-- Define a global box around the canvas
Global {
   shape box = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      strokeColor: rgba(0.,0.,0.,.05)
      fillColor: none()
      strokeWidth: 1
   }
}

-- Draw each word as text inside a
-- rounded rectangle.  Note that this
-- rule will get applied to all Words,
-- of any subtype.
forall Word w {

   -- Draw the text
   w.text = Text {
      center: (?,?) -- the center location will be optimized by Penrose
      string: w.label -- the label comes from the Substance program
      fillColor: Colors.black
      fontFamily: "Palatino"
      fontSize: "9px" -- make sure to use px, not pt, since pt appears inconsistently across programs/browsers
   }

   -- Draw the rectangle
   scalar padding = 8
   w.box = Rectangle {
      -- Use the same center, width, and
      -- height as the text, but add some padding
      center: w.text.center
      width: w.text.width + padding
      height: w.text.height + padding

      fillColor: Colors.lightGray
      strokeColor: Colors.black
      strokeWidth: .5
      cornerRadius: 5
   }

   -- Make sure the rectangle is always on the
   -- canvas (since the rectangle surrounds the text,
   -- the text will also then be on the canvas).
   ensure contains( Global.box, w.box )

   -- Draw the rectangle and text above the canvas (just in case
   -- the canvas is drawn using, e.g., an opaque fill color).
   layer w.box above Global.box
   layer w.text above Global.box
}

-- Set specific colors for specific types of text
forall AWord w {
   override w.box.fillColor = Colors.lightBlue
}
forall BWord w {
   override w.box.fillColor = Colors.lightGreen
}

-- Make sure no text boxes overlap
forall Word w1; Word w2 {
   ensure disjoint( w1.box, w2.box, 3. )
}

-- Encourage words of the same kind to be close to each-other
forall AWord w1; AWord w2 {
   encourage near( w1.box, w2.box )
}
forall BWord w1; BWord w2 {
   encourage near( w1.box, w2.box )
}


`,t=`type Word

type AWord <: Word -- word related to topic A
type BWord <: Word -- word related to topic B
`,s={substance:n,style:[{contents:r,resolver:o}],domain:t,variation:"JazlynMouse32496"};export{s as default};
//# sourceMappingURL=example.trio-ca91736b.js.map
