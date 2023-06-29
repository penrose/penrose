import{m as n}from"./resolver-16fd6907.js";import"./iframe-c84affa1.js";const o=`/* This is the starter code for the substance program for tutorial 3,
 * which covers functions in Penrose. Follow along with the write-up. 
 * Good luck! :)
 */
 
VectorSpace U
Vector v 
Vector w
In(v, U)
In(w, U)

-- Start your code below


-- End your code above
AutoLabel All /* leave this as the last line */`,e=n("tutorials/code/tutorial3"),r=`/* This is the starter code for the style program for tutorial 3,
 * which covers functions in Penrose. Scroll down to the
 * bottom and follow along with the write-up. Good luck! :)
 */

/* here are some useful constants that we use to draw
 * the vector space
 */
canvas {
  width = 800
  height = 700
}

const {
  scalar vectorSpaceSize = 350.0
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  scalar arrowThickness = 1.5
  color gray = rgba(0.6, 0.6, 0.6, 1.)
  color lightBlue = rgba(0.2, 0.4, 0.8, 1.0)
  color lightGray = rgba(252, 252, 252, 0.015)
  color green = rgba(0., 0.8, 0., 1.)
  color none = rgba(0., 0., 0., 0.)
}

/* here we draw a vector space by defining an origin
 * of the vector space, and x-axis, y-axis that are
 * centered at the origin
 */
forall VectorSpace U {
    scalar axisSize = const.vectorSpaceSize / 2.0
    vec2 U.origin = (0., 0.)
    vec2 o = U.origin /* just so we don't need to type U.origin everytime */
    U.axisColor = const.gray

    U.background = Rectangle {
        center : U.origin
        width : const.vectorSpaceSize
        height : const.vectorSpaceSize
        fillColor : const.lightGray
        strokeColor : const.none
    }

    U.xAxis = Line {
        start : (o[0] - axisSize, o[1])
        end : (o[0] + axisSize, o[1])
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: "straight"
        endArrowhead: "straight"
        startArrowheadSize : const.arrowheadSize * 2.
        endArrowheadSize : const.arrowheadSize * 2.
    }

    U.yAxis = Line {
        start : (o[0], o[1] - axisSize)
        end : (o[0], o[1] + axisSize)
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: "straight"
        endArrowhead: "straight"
        startArrowheadSize : const.arrowheadSize * 2.
        endArrowheadSize : const.arrowheadSize * 2.
    }

    U.text = Equation {
        string : U.label
        center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
        fillColor : U.axisColor
    }
}

-- Start your code below

-- End your code above
`,t=`/* This is the starter code for the domain program for tutorial 3,
 * which covers functions in Penrose. Follow along with the write-up.
 * Good luck! :)
 */

type VectorSpace
type Vector
predicate In(Vector, VectorSpace V)

-- Start your code below

-- End your code above
`,s={substance:o,style:[{contents:r,resolver:e}],domain:t,variation:""};export{s as default};
//# sourceMappingURL=tutorial3.trio-bb36c151.js.map
