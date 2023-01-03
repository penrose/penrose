<script setup lang="ts">
import shapeDefs from "../shapedefs.json";
import PropValue from "./PropValue.vue";

const { shapeName } = defineProps<{ shapeName: string }>();
const thisShape = shapeDefs[shapeName];
</script>

<template>
  <table>
    <thead>
      <tr>
        <th>Property</th>
        <th>Type</th>
        <th>Default</th>
      </tr>
    </thead>
    <tbody>
      <tr v-for="(propValue, propName) in thisShape['defaulted']">
        <td>{{ propName }}</td>
        <td>{{ propValue.tag }}</td>
        <td><PropValue v-bind="propValue" /></td>
      </tr>
      <tr v-for="(propValue, propName) in thisShape['sampled']">
        <td>{{ propName }}</td>
        <td>{{ propValue.tag }}</td>
        <td>
          <span style="font-style: italic">sampled</span>
        </td>
      </tr>
    </tbody>
  </table>
</template>
