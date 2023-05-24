<script setup>
import { constrDict, compDict, objDict } from "@penrose/core"
import Function from "../../../src/components/Function.vue"
</script>

# Style Functions

## Constraint Functions

<div v-for="(f, index) in constrDict" :key="index">

### {{ f.name }}

<ClientOnly>
<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</ClientOnly>

</div>

## Objective Functions

<div v-for="f in objDict">

### {{ f.name }}

<ClientOnly>
<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</ClientOnly>

</div>

## Computation Functions

<div v-for="f in compDict">

### {{ f.name }}

<ClientOnly>
<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</ClientOnly>

</div>
