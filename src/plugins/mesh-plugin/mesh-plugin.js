const Fs = require('fs');
const _ = require('lodash');

// Geometry processing imports
const Vertex = require('./geometry-processing-js/node/core/vertex.js')
const Edge = require('./geometry-processing-js/node/core/edge.js')
const Halfedge = require('./geometry-processing-js/node/core/halfedge.js')
const MeshSubset = require('./geometry-processing-js/node/core/mesh-subset.js')
const Mesh = require('./geometry-processing-js/node/core/mesh.js')
const Geometry = require('./geometry-processing-js/node/core/geometry.js')
const MeshIO = require('./geometry-processing-js/node/utils/meshio.js');

const SC = require('./geometry-processing-js/node/projects/simplicial-complex-operators/simplicial-complex-operators.js');
// TODO: port simplicial complex project to Node

// Input meshes
// const FaceMesh = require('./geometry-processing-js/input/face.js');
// const HexMesh = require('./geometry-processing-js/input/hexagon.js');
// const DiskMesh = require('./geometry-processing-js/input/small_disk.js');
const TetraMesh = require('./geometry-processing-js/input/tetrahedron.js');

global_mesh = undefined;
newline = "\n";

function vname(cname, v) {
    return cname + "_v" + v.index;
}

function ename(cname, e) {
    return cname + "_e" + e.index;
}

function vdecl(cname, v) {
    return "Vertex " + vname(cname, v);
}

function edecl(cname, e) {
    return "Edge " + ename(cname, e);
}

function inVE(cname, v, e) {
    return "InVE(" + vname(cname, v) + ", " + ename(cname, e) + ")";
}

function mkE(cname, v1, v2) {
    return "e := MkEdge(" + vname(cname, v1) + ", " + ename(cname, v2) + ")";
}

function makeSComplex(cname) {
    // TODO: maintain a lookup table for objects that belong to c

    // TODO: make a simple random mesh, not a constant one
    let polygonSoup = MeshIO.readOBJ(TetraMesh);
    let mesh = new Mesh.Mesh();
    mesh.build(polygonSoup);

    // TODO: keep this mesh alive

    // -------------------------------------------
    // TODO: factor out the simplicial complex code
    let SCO = new SC.SimplicialComplexOperators(mesh);

    let selectedSimplices = new MeshSubset();
    selectedSimplices.addVertices([1,2,3]);
    selectedSimplices.addEdges([1,2,3]);
    selectedSimplices.addFaces([1,2,3]);

    let star_sc = SCO.star(selectedSimplices);
    console.log("sc: ", SCO);
    console.log("sc star: ", star_sc);
    // -------------------------------------------

    global_mesh = mesh; // TODO remove

    let prog = [];

    // Emit all declarations first, since objects need to be defined in Substance before they're referenced
    for (let v of mesh.vertices) {
	prog.push(vdecl(cname, v));
    }

    for (let e of mesh.edges) {
	prog.push(edecl(cname, e));
    }

    // Build connectivity info
    for (let v of mesh.vertices) {
	for (let e of v.adjacentEdges()) {
	    prog.push(inVE(cname, v, e));
	}
    }

    return prog;
}

function makeSub(json) {
    // We only construct simplicial complexes, and objects are named according to the SComplex they belong to
    let scNames = json.objects.filter(o => o.objType === 'SComplex').map(o => o.objName);
    console.log("complexes", scNames);

    // TODO: use consistent fp style
    let complexesProg = _.flatten(scNames.map(name => makeSComplex(name)));

    return complexesProg.join(newline);
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
