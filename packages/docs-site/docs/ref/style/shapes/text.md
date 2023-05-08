<script setup>
import ShapeProps from "../../../../src/components/ShapeProps.vue";
</script>

# Text

A `Text` shape is used to display plain text; see `Equation` for mathematical expressions. The `Text` shape supports many of the same attributes as SVG text. Note that by default, all text is centered vertically and horizontally, via the [`textAnchor`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-anchor) and [`alignmentBaseline`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline) fields (which can be changed to left, bottom, etc.).

The width and height of every `Text` shape is automatically set to the width/height of its bounding box—you do not have to set these values. In fact, it is often convenient to use these values when defining other shapes. For instance, to draw a `Rectangle` around an existing `Text` shape called `t`, you can set the rectangle's width/height/center fields to `width: t.width`, `height: t.height`, and `center: t.center`. (Note that this rectangle will be properly centered on the text only if the text retains the default values `textAnchor: middle` and `alignmentBaseline: alphabetic`; changing these values may cause the two shapes to become misaligned).

## Properties

<ShapeProps shape-name="Text" />
