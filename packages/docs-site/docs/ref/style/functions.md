---
anchors: functions
---

<script setup lang="ts">
import Function from "../../../src/components/Function.vue"
import { data } from "./functions.data.js"
const { objDict, compDict, constrDict } = data;
</script>

# Style Functions

## Constraint Functions

<div v-for="f in constrDict">

<h3 :id="`constraint-${f.name.toLowerCase()}`">
  {{ f.name }}
  <a class="header-anchor" :href="`#constraint-${f.name.toLowerCase()}`" :aria-label="`Permalink to constraint &quot;${f.name}&quot;`">&ZeroWidthSpace;</a>
</h3>

<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />

</div>

## Objective Functions

<div v-for="f in objDict">

<h3 :id="`objective-${f.name.toLowerCase()}`">
  {{ f.name }}
  <a class="header-anchor" :href="`#objective-${f.name.toLowerCase()}`" :aria-label="`Permalink to objective &quot;${f.name}&quot;`">&ZeroWidthSpace;</a>
</h3>

<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />

</div>

## Computation Functions

<div v-for="f in compDict">

<h3 :id="`computation-${f.name.toLowerCase()}`">
  {{ f.name }}
  <a class="header-anchor" :href="`#computation-${f.name.toLowerCase()}`" :aria-label="`Permalink to computation &quot;${f.name}&quot;`">&ZeroWidthSpace;</a>
</h3>

<Function :name="f.name" :description="f.description" :params="f.params" :returns="f.returns" />

</div>
