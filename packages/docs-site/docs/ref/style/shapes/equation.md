<script setup>
import ShapeProps from "../../../../src/components/ShapeProps.vue";
</script>

# Equation

An `Equation` is used to typeset mathematical expressions. It assumes that the `string` field corresponds to a math mode TeX expression, and uses MathJax to render this string. Note that ordinary (non-math) text can be drawn via the `Text` shape.

## Properties

<ShapeProps shape-name="Equation" />
