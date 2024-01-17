<script setup>
import ShapeProps from "../../../../src/components/ShapeProps.vue";
import Embed from "../../../../src/components/Embed.vue";
import arrowheads from "@penrose/examples/dist/shape-spec/arrowheads.trio.js"

const arrows = {
  trio: {
    substance: arrowheads.substance,
    style: arrowheads.style[0].contents,
    domain: arrowheads.domain,
    variation: arrowheads.variation,
  },
  imageResolver: arrowheads.style[0].resolver,
}

</script>

# Line

<ShapeProps shape-name="Line" />

The `Line` shape allows a few styles of arrowheads arrowhead styles inspired by [quiver](https://q.uiver.app/). The arrowhead styles are given by `startArrowhead` and `endArrowhead`. The example below shows all the supported options:

<Embed :trio=arrows.trio  />
