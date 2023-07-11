<script setup lang="ts">
import Function from "../../../src/components/Function.vue"
import { data } from "./functions.data.js"
const { objDict, compDict, constrDict } = data;
</script>

# Style Functions

## Constraint Functions

<div v-for="f in constrDict">

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
