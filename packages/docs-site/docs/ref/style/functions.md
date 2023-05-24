<script setup>
import Function from "../../../src/components/Function.vue"
import {ref, onMounted} from "vue"

const constrs = ref({})
const objs = ref({})
const comps = ref({})

onMounted(async () => {
  const { constrDict, objDict, compDict } = await import("@penrose/core")
  constrs.value = constrDict
  objs.value = objDict
  comps.value = compDict
})

</script>

# Style Functions

## Constraint Functions

<div v-for="(f, index) in constrs" :key="index">

### {{ f.name }}

<ClientOnly>
<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</ClientOnly>

</div>

## Objective Functions

<div v-for="f in objs">

### {{ f.name }}

<ClientOnly>
<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</ClientOnly>

</div>

## Computation Functions

<div v-for="f in comps">

### {{ f.name }}

<ClientOnly>
<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />
</ClientOnly>

</div>
