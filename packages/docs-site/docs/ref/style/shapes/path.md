<script setup>
import ShapeProps from "../../../../src/components/ShapeProps.vue";
</script>

# Path

## Properties

<ShapeProps shape-name="Path" />

## Defining an SVG path using `d`

The `d` attribute should receive information about the particular points you want to draw your path on. Currently, `Path` can handle SVG commands for lines and arcs. For detailed information about the information that each SVG command expects, check out [this](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d) resource.
In your style program, to render each of the following types of SVG commands, pass `d` the return value of the following functions:

- **Line or series of connected line segments**: `pathFromPoints(complete, [[x1, y1], [x2, y2],...,[xn, yn]])` where:
  - `complete`: `"open"` to keep the SVG path open, `"closed"` to draw a line from the last point to the first
  - `[[x1, y1], [x2, y2],...,[xn, yn]]` is the list of ordered points to make up the SVG path
- **Arc**: `arc(complete, start, end, radius, rotation, largeArc, arcSweep)` (well-illustrated documentation on the purpose of each of the arc attributes in SVG can be found [here](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths#arcs)):
  - `complete`: `"open"` to keep the SVG path open, `"closed"` to draw a line from the last point to the first
  - `start`: Coordinate `[x,y]` representing the beginning of the arc
  - `end`: Coordinate `[x,y]` representing the endpoint of the arc
  - `radius`: List of length 2 `[rx, ry]`, where `rx` is the radius of the ellipse to draw the arc along in the `x` direction (i.e. the width), and `ry` is the radius of the ellipse in the `y` direction.
  - `rotation` Number representing the degree of rotation of the ellipse
  - `largeArc`: `0` to draw shorter of 2 arcs, `1` to draw the longer
  - `arcSweep`: `0` to draw in the counter-clockwise direction from `start` to `end`, `1` to draw in the clockwise direction
- **Curve passing through three points**: `interpolateQuadraticFromPoints(pathType,p0,p1,p2)` where:
  - `pathType`: `"open"` to keep the SVG path open, `"closed"` to draw a line from the last point to the first
  - `p0`: start point
  - `p1`: middle point — unlike a standard quadratic Bézier curve, which will _not_ pass through the middle control point, this curve will actually pass exactly through this point.
  - `p2`: end point
