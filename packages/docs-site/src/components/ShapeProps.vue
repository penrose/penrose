<script setup lang="ts">
import shapeDefs from "../shapedefs.json";

/**
 * Shows the value of a shape property.
 *
 * @param propValue The shape's property value
 * @returns string representing the value
 */
const showValue = (propValue: { contents: any; tag: string }) => {
  const contents = propValue.contents;

  if (typeof contents === "object") {
    switch (contents.tag) {
      case "NONE":
        return "NONE";
      case "RGBA": {
        const arr = contents.contents;
        return `rgba(${arr[0]},${arr[1]},${arr[2]},${arr[3]})`;
      }
    } // switch: contents.tag
    if ("val" in contents) {
      return JSON.stringify(contents.val);
    }
  } // if: object
  return JSON.stringify(contents);
};

defineProps<{
  shapeName: string;
}>();
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
      <tr
        v-for="[propName, propVal] in Object.entries(
          shapeDefs[shapeName]['defaulted']
        )"
      >
        <td>{{ propName }}</td>
        <td>{{ propVal.tag }}</td>
        <td>{{ showValue(propVal) }}</td>
      </tr>
      <tr
        v-for="[propName, propVal] in Object.entries(
          shapeDefs[shapeName]['sampled']
        )"
      >
        <td>{{ propName }}</td>
        <td>{{ propVal.tag }}</td>
        <td>
          <span :style="{ fontStyle: 'italic' }">sampled</span>
        </td>
      </tr>
    </tbody>
  </table>
</template>
