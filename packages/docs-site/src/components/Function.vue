<script lang="ts">
import { defineComponent } from "vue";
import Markdown from "markdown-it";
import { describeType } from "@penrose/core";
const md = new Markdown();
export default defineComponent({
  props: ["name", "description", "params", "returns"],
  computed: {
    desc() {
      return md.render(this.description ?? "");
    },
    parameters() {
      return this.params.map((p: any) => ({
        ...p,
        type: describeType(p.type),
      }));
    },
  },
});
</script>

<template>
  <h3>
    {{ name }}
    <a class="header-anchor" href="#name" aria-hidden="true">#</a>
  </h3>
  <div v-html="desc"></div>
  <div v-if="parameters.length > 0">
    <h4>Parameters:</h4>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Type Description</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="param in parameters">
          <td>
            <code>{{ param.name }}</code>
          </td>
          <td>{{ param.type.symbol }}</td>
          <td>{{ param.type.description }}</td>
          <td>{{ param.description }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>
