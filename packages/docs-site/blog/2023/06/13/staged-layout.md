<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
import StagedDiagram from "../../../../src/components/StagedDiagram.vue";
import { ref } from 'vue'
import vector from "@penrose/examples/dist/exterior-algebra/vector-wedge.trio";
import laplace from "@penrose/examples/dist/walk-on-spheres/laplace-estimator.trio.js";
import geometry from "@penrose/examples/dist/geometry-domain/textbook_problems/c05p13.trio";
import euclideanOneStage from "./euclideanOneStage"
import { defineComponent } from "vue";
import { applyPureReactInVue } from "veaury";

const exterior = {
  trio: {
    substance: vector.substance,
    style: vector.style[0].contents,
    domain: vector.domain,
    variation:"ArtemisCrane740"
  },
  imageResolver: vector.style[0].resolver,
}
const wos = {
  trio: {
    substance: laplace.substance,
    style: laplace.style[0].contents,
    domain: laplace.domain,
    variation: "test3",
  },
  imageResolver: laplace.style[0].resolver,
}
const incenterOneStage = {
  trio: {
    substance: geometry.substance,
    style: euclideanOneStage,
    domain: geometry.domain,
    variation: "test8",
  },
  imageResolver: geometry.style[0].resolver,
}
const incenter = {
  trio: {
    substance: geometry.substance,
    style: geometry.style[0].contents,
    domain: geometry.domain,
    variation: "test20",
  },
  imageResolver: geometry.style[0].resolver,
}
</script>

# Diagram Layout in Stages

<BlogMeta github="wodeni" date="2023-06-13" />

In Style, we can write `ensure` and `encourage` statements to declare constraints and objectives. The compiler turns them into a numerical optimization problem, and the optimizer finds a good layout. So far, this approach has worked fairly well for us. As we author more complex diagrams in Penrose, however, we ran into more difficult optimization problems.

<div>
For example, we have built a series of diagrams in 2D Euclidean geometry. Initially, the layout engine worked just fine. But as the Style program become a lot more complex, the optimizer started to struggle. Take this diagram as an example.

<div style="width: 55%; float:right; margin-left: 16px; margin-top: 16px">
  <StagedDiagram :trio="incenter.trio" :imageResolver="incenter.imageResolver" />
  <StagedDiagram :trio="incenterOneStage.trio" :imageResolver="incenterOneStage.imageResolver" />
</div>

Conceptually, this diagram illustrates the _incenter_ $P$ of $\triangle JKL$, where $P$ is equidistant from the triangle's sides. The layout problem can be broken down into:

- Lay out points $J$, $K$, and $L$ to form a [non-degenerate triangle](<https://en.wikipedia.org/wiki/Degeneracy_(mathematics)#Triangle>).
- Compute the location of the incenter $P$ of $\triangle JKL$.
- Lay out point $m$ on $\overline{KL}$.
- Lay out $\overline{mP}$ perpendicular to $\overline{KL}$.
- Lay out all point labels so they are close to the points.
  - If the label is for a triangle vertex, make sure it's outside of the triangle.

Aside from the location of $P$, everything else is optimized. And as you can see, the optimizer attempts to satisfy **all of the above concurrently**. Notice in every frame of the layout animation, Penrose always tries to keep the labels close to the points.

</div>

<!-- <StagedDiagram :trio="exterior.trio" :imageResolver="exterior.imageResolver" /> -->
<!-- <StagedDiagram :trio="incenter.trio" :imageResolver="incenter.imageResolver" /> -->

## Constraint-based layout

## The Stage Syntax

The user can annotate varying values (i.e. `?`), constraints, and objectives with layout stages. At layout time, the optimizer executes layout stages in order. For instance, the user may want to layout labels before laying out shapes (e.g. #1115), and using the feature, they can separate varying values and constraints into distinct sets to be optimized in stages.

The layout stages are optionally defined at the top level in Style:

```style
layout = [ShapeLayout, LabelLayout, Overall]
```

If undefined, `layout` gets a default anonymous stage, i.e. the default mode where all inputs and constraints are jointly optimized at once.

In Style blocks, inputs and constraints can be associated with stages via the `in` keyword:

```style
forall Object o {
  o.shape = Square {
    -- `in` can be used after any `?`
    center : [? in ShapeLayout, ? in ShapeLayout]
    -- use brackets after `in` for multiple stages
    w: ? in [ShapeLayout, Overall]
  }
  o.text = Text {
    center: [? in LabelLayout, ? in LabelLayout]
  }
  ensure contains(o.shape, o.text) in [LabelLayout, Overall]
  encourage equal(o.shape.center[1]. o.text.center[1]) in LabelLayout
  -- if let unspecified, the constraint/objective/input automatically participate in all stages,
  -- i.e. [ShapeLayout, LabelLayout, Overall]
  ensure onCanvas(o.shape)
}
```

<StagedDiagram :trio="wos.trio" :imageResolver="wos.imageResolver" />
