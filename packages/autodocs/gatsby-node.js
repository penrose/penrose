const { shapedefs } = require("@penrose/core");
const path = require("path");
exports.sourceNodes = ({ actions, createNodeId, createContentDigest }) => {
  shapedefs.forEach((shape) => {
    const node = {
      name: shape.shapeType,
      properties: Object.entries(shape.properties).map(
        ([name, { propType }]) => ({
          name,
          propType,
        })
      ),
      id: createNodeId(`shapeDef-${shape.shapeType}`),
      internal: {
        type: "Shape",
        contentDigest: createContentDigest(shape),
      },
    };
    actions.createNode(node);
  });
};

exports.createPages = async ({ graphql, actions, reporter }) => {
  const { createPage } = actions;
  const result = await graphql(`
    query {
      allShape {
        nodes {
          id
          name
        }
      }
    }
  `);
  const ShapePage = path.resolve(`src/templates/shapePage.js`);
  result.data.allShape.nodes.forEach(({ id, name }) => {
    createPage({
      path: `/shape/${name}`,
      component: ShapePage,
      context: {
        id,
      },
    });
  });
};
