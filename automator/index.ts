const fs = require("fs");
const mathjax = require("mathjax-node");
const { propagateUpdate } = require("../react-renderer/src/PropagateUpdate");

const args = process.argv.slice(2);

const st = fs.readFileSync("./state.json");

const state = JSON.parse(st).contents[0];

const allShapes = state.shapesr;

(async () => {
  const collected = await Promise.all(
    allShapes.map(async ([type, obj]) => {
      if (type === "Text" || type === "TextTransform") {
        const data = await mathjax.typeset({
          math: obj.string.contents,
          format: "TeX",
          svg: true,
          svgNode: true,
          useFontCache: false,
          useGlobalCache: false,
          ex: 1
        });
        if (data.errors) {
          console.error(
            `Could not render ${obj.string.contents}: `,
            data.errors
          );
          return;
        }
        const { width, height } = data;
        const obj2 = { ...obj };
        // Take substring to omit `ex`
        obj2.w.updated = width.substring(width.length - 2);
        obj2.h.updated = height.substring(height.length - 2);

        return [type, obj, data.svgNode];
      } else {
        return [type, obj];
      }
    })
  );
  const updated = await propagateUpdate({ ...state, shapesr: collected });
  console.log(updated);
})();
