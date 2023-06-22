<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
</script>

# Diagram Layout in Stages

<BlogMeta github="wodeni" date="2023-06-13" />

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
