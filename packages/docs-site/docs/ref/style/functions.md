<script setup>
import { constrDict, compDict, objDict } from "@penrose/core"
import Function from "../../../src/components/Function.vue"
import markdownit from "markdown-it"
</script>

# Style Functions

## Constraint Functions

<div v-for="f in constrDict">
  <Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</div>

## Objective Functions

<div v-for="f in objDict">
  <Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</div>

## Computation Functions

<div v-for="f in compDict">
  <Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</div>
