<script setup lang="ts">
/**
 * Shows the value of a shape property.
 *
 * @param propValue The shape's property value
 * @returns data representing the value
 */
const showValue = (contents: any): { text: string; color?: string } => {
  if (typeof contents === "object") {
    switch (contents.tag) {
      case "NONE":
        return { text: "NONE" };
      case "RGBA": {
        const c = contents.contents;
        return {
          color: `rgba(${c[0] * 255},${c[1] * 255},${c[2] * 255},${c[3]})`,
          text: `rgba(${c[0]},${c[1]},${c[2]},${c[3]})`,
        };
      }
    } // switch: contents.tag
    if ("val" in contents) {
      return { text: JSON.stringify(contents.val) };
    }
  } // if: object
  return { text: JSON.stringify(contents) };
};

const { contents } = defineProps<{ contents: any; tag: string }>();
const { text, color } = showValue(contents);
</script>

<template>
  <div v-if="color">
    <div
      :style="{
        display: 'inline',
        border: '1px solid gray',
        backgroundColor: color,
        borderRadius: '5px',
      }"
    >
      &nbsp;&nbsp;&nbsp;&nbsp;
    </div>
    <span style="margin-left: 1em">{{ text }}</span>
  </div>
  <template v-else>{{ text }}</template>
</template>
