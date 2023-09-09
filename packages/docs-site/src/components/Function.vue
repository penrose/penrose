<script lang="ts">
import { defineComponent } from "vue";
export default defineComponent({
  props: ["name", "description", "params", "returns"],
});
</script>

<template>
  <div v-html="description"></div>
  <div v-if="params.length > 0">
    <h4>Parameters:</h4>
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Type Description</th>
          <th>Description</th>
          <th v-if="params.filter((p) => p.default).length > 0">
            Default Value
          </th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="param in params">
          <td>
            <code>{{ param.name }}</code>
          </td>
          <td>{{ param.type.symbol }}</td>
          <td>{{ param.type.description }}</td>
          <td v-html="param.description"></td>
          <td v-if="params.filter((p) => p.default).length > 0">
            {{ param.default ?? "" }}
          </td>
        </tr>
      </tbody>
    </table>
  </div>
  <div v-if="returns">
    <h4>Returns: {{ returns.symbol }} ({{ returns.description }})</h4>
  </div>
</template>
