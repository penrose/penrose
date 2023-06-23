<script lang="ts">
import { defineComponent } from "vue";
import Markdown from "markdown-it";
import { describeType } from "@penrose/core";
import { FuncParam } from "@penrose/core/dist/types/functions";
const md = new Markdown();
export default defineComponent({
  props: ["name", "description", "params", "returns"],
  computed: {
    desc() {
      return md.render(this.description ?? "");
    },
    ret() {
      return describeType(this.returns);
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
          <th v-if="parameters.filter((p: FuncParam) => p.default).length > 0">
            Default Value
          </th>
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
          <td v-if="parameters.filter((p: FuncParam) => p.default).length > 0">
            {{ param.default ?? "" }}
          </td>
        </tr>
      </tbody>
    </table>
  </div>
  <div v-if="returns">
    <h4>Returns: {{ ret.symbol }} ({{ ret.description }})</h4>
  </div>
</template>
