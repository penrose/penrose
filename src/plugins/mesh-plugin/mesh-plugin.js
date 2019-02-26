const fs = require('fs');
const gp_vertex = require('./geometry-processing-js/node/core/vertex.js')
const gp_edge = require('./geometry-processing-js/node/core/mesh.js')
const gp_halfedge = require('./geometry-processing-js/node/core/mesh.js')
const gp_mesh = require('./geometry-processing-js/node/core/mesh.js')
const gp_geometry = require('./geometry-processing-js/node/core/geometry.js')

function makeSub(json) {
    let m1 = new gp_mesh[0]();
    console.log(m1);
    let newSub = "Vertex v1\nVertex v2\nEdge e\ne := MkEdge(v1, v2)";
    return newSub;
}

function main() {
   console.log("starting mesh plugin");

   // https://stackabuse.com/reading-and-writing-json-files-with-node-js/
   let rawdata = fs.readFileSync('Sub_enduser.json');
   let subJSON = JSON.parse(rawdata);
   console.log("Received JSON", subJSON);

   let newSub = makeSub(subJSON)
   console.log("writing data: ", newSub);
   fs.writeFileSync('Sub_instantiated.sub', newSub);

   console.log("ending mesh plugin")
}

main();
