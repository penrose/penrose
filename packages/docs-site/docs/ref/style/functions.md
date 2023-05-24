<script setup>
import Function from "../../../src/components/Function.vue"
import { constrDict, objDict, compDict } from "@penrose/core"
</script>

# Style Functions

## Constraint Functions

<div v-for="(f, index) in constrDict" :key="index">

### {{ f.name }}

<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />

</div>

## Objective Functions

<div v-for="f in objDict">

### {{ f.name }}

<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />

</div>

## Computation Functions

<div v-for="f in compDict">

### {{ f.name }}

<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />

</div>
