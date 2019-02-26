const Fs = require('fs');
const Vertex = require('./geometry-processing-js/node/core/vertex.js')
const Edge = require('./geometry-processing-js/node/core/mesh.js')
const Halfedge = require('./geometry-processing-js/node/core/mesh.js')
const Mesh = require('./geometry-processing-js/node/core/mesh.js')
const Geometry = require('./geometry-processing-js/node/core/geometry.js')
const MeshIO = require('./geometry-processing-js/node/utils/meshio.js');
// TODO: port simplicial complex project to Node

// Input meshes
// const FaceMesh = require('./geometry-processing-js/input/face.js');
// const HexMesh = require('./geometry-processing-js/input/hexagon.js');
// const DiskMesh = require('./geometry-processing-js/input/small_disk.js');
const TetraMesh = require('./geometry-processing-js/input/tetrahedron.js');

global_mesh = undefined;
newline = "\n";

function vname(v) {
    return "v" + v.index;
}

function ename(e) {
    return "e" + e.index;
}

function vdecl(v) {
    return "Vertex " + vname(v);
}

function edecl(e) {
    return "Edge " + ename(e);
}

function inVE(v, e) {
    return "InVE(" + vname(v) + ", " + ename(e) + ")";
}

function mkE(v1, v2) {
    return "e := MkEdge(" + vname(v1) + ", " + ename(v2) + ")";
}

function makeSub(json) {
    // TODO: do something as function of json

    let polygonSoup = MeshIO.readOBJ(TetraMesh);
    let m1 = new Mesh.Mesh();
    m1.build(polygonSoup);
    console.log(m1);

    global_mesh = m1;

    // vertices, edges, faces, corners, halfedges, boundaries
    let vs = m1.vertices;

    let prog = [];

    // Emit all declarations first, since objects need to be defined in Substance before they're referenced
    for (let v of m1.vertices) {
	prog.push(vdecl(v));
    }

    for (let e of m1.edges) {
	prog.push(edecl(e));
    }

    // Build connectivity info
    for (let v of m1.vertices) {
	for (let e of v.adjacentEdges()) {
	    prog.push(inVE(v, e));
	}
    }

    return prog.join(newline);
}

function main() {
   console.log("starting mesh plugin");

   let rawdata = Fs.readFileSync('Sub_enduser.json');
   let subJSON = JSON.parse(rawdata);
   console.log("Received JSON", subJSON);

   let newSub = makeSub(subJSON)
   console.log("writing data: ", newSub);
   Fs.writeFileSync('Sub_instantiated.sub', newSub);

   console.log("ending mesh plugin")
}

main();

module.exports = {main, makeSub, global_mesh};
