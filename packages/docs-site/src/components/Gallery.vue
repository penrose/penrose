<template>
  <div class="container">
    <a
      v-for="example in examples"
      :key="example.id"
      :href="`${ideLink}?examples=${example.id}`"
      target="_blank"
    >
      <div :class="['example-container', { dark }]">
        <div v-html="example.preview"></div>
      </div>
    </a>
  </div>
</template>

<script>
import { withBase } from "vitepress";
const ideLink = withBase(`/try/index.html`);
import { ref, onMounted } from "vue";
// crop the SVG if necessary, i.e. if the
// cropped view box is smaller than the current view box
export const cropSVG = (svg) => {
  const parser = new DOMParser();
  const serializer = new XMLSerializer();
  const svgDoc = parser.parseFromString(svg, "image/svg+xml");
  const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
  const svgNode = svgDoc.querySelector("svg");
  const viewBox = svgNode.getAttribute("viewBox");
  if (cropped && viewBox) {
    const viewBoxNums = svgNode.viewBox.baseVal;
    const croppedNums = cropped.split(/\s+|,/);

    const croppedWidth = parseFloat(croppedNums[3]);
    const croppedHeight = parseFloat(croppedNums[4]);
    const viewBoxWidth = viewBoxNums.width;
    const viewBoxHeight = viewBoxNums.height;

    // if area of cropped view box is leq area of current view box
    // then set the view box to the cropped view box
    if (
      Math.abs(croppedWidth * croppedHeight) <
      Math.abs(viewBoxWidth * viewBoxHeight)
    ) {
      svgNode.setAttribute("viewBox", cropped);
    }
  }
  return serializer.serializeToString(svgNode);
};

export default {
  props: {
    trios: Array,
    ideLink: String,
    dark: Boolean,
  },
  setup(props) {
    const examples = ref(props.trios.map((id) => ({ id })));

    onMounted(async () => {
      const load = async () => {
        for (const id of props.trios) {
          const svg = await fetch(
            encodeURI(
              `https://raw.githubusercontent.com/penrose/penrose/ci/refs/heads/main/${id}.svg`
            )
          );
          if (svg.ok) {
            const preview = await svg.text();
            const croppedPreview = cropSVG(preview);
            const trio = { id, preview: croppedPreview };
            examples.value = examples.value.map((e) =>
              e.id === id ? { ...trio, preview: croppedPreview } : e
            );
          }
        }
      };
      load();
    });

    return {
      examples,
      ideLink: ideLink,
    };
  },
};
</script>

<style scoped>
.container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  align-items: center;
  justify-items: center;
  padding: 2em;
  grid-gap: 1.5em;
}

.example-container {
  width: 200px;
  height: 200px;
  padding: 1.5em;
  background-color: #fff;
  border-radius: 10px;
  box-shadow: 0px 3px 3px rgba(0, 0, 0, 0.1);
  transition: 0.3s;
}

.example-container:hover {
  box-shadow: 0px 10px 10px rgba(0, 0, 0, 0.2);
  transform: scale(1.05);
}

.dark .example-container:hover {
  box-shadow: 0px 1px 2px 0px rgba(46, 154, 216, 0.7),
    1px 2px 4px 0px rgba(46, 154, 216, 0.7),
    2px 4px 8px 0px rgba(46, 154, 216, 0.7),
    2px 4px 16px 0px rgba(46, 154, 216, 0.7);
}
</style>
