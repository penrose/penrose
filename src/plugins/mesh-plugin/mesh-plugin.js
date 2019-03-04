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
vtype = "Vertex";
etype = "Edge";
ftype = "Face";

function objName(cname, objType, index) {
    return cname + "_" + tstr(objType) + index;
}

function label(cname, objType, index) {
    return "Label " + objName(cname, objType, index) 
	+ " $" + cname[0] + "_{" + cname.substring(1) + "}"
	+ " v_{" + index + "}$";

}

function decl(cname, objType, index) {
    return objType + " " + objName(cname, objType, index);
}

function inVE(cname, vi, ei) {
    return "InVE(" + objName(cname, vtype, vi) + ", " + objName(cname, etype, ei) + ")";
}

function mkE(ename, cname, vi, vj) {
    return ename + " := MkEdge(" + objName(cname, vtype, vi) + ", " 
	+ objName(cname, vtype, vj) + ")";
}

function mkF(fname, cname, ei, ej, ek) {
    return fname + " := MkFace(" + objName(cname, etype, ei) + ", " 
	+ objName(cname, etype, ej) + ", "
	+ objName(cname, etype, ek) + ")";
}

function tstr(objType) {
    if (objType === vtype) {
	return "V";
    } else if (objType === etype) {
	return "E";
    } else if (objType === ftype) {
	return "F";
    } else {
	console.log("objType not recognized: ", objType);
	return "ERROR";
    }
}

function selected(objType, objName) {
    return "Selected" + tstr(objType) + "(" + objName + ")";
}

function makeSComplex(cname) {
    // TODO: maintain a lookup table for objects that belong to c

    // TODO: make a simple random mesh, not a constant one
    // Tetrahedron has vertices indexed from 0-3 (inclusive), edges 0-5, faces 0-3
    // Maybe I should draw a 2D mesh...
    let polygonSoup = MeshIO.readOBJ(TetraMesh);
    let mesh = new Mesh.Mesh();
    mesh.build(polygonSoup);

    // TODO: keep this mesh object outside of function scope

    // Build mappings from Penrose name to mesh index (not really needed since each name includes its index...)
    let penrose_to_mesh = {};

    // -------------------------------------------
    // TODO: factor out the simplicial complex code
    let SCO = new SC.SimplicialComplexOperators(mesh);

    let selectedSimplices = new MeshSubset();
    selectedSimplices.addVertices([1]);
    selectedSimplices.addEdges([]);
    selectedSimplices.addFaces([]);

    let star_sc = SCO.star(selectedSimplices);
    console.log("sc: ", SCO);
    console.log("sc star: ", star_sc);
    // -------------------------------------------

    global_mesh = mesh; // TODO remove

    let prog = [];

    // Emit all declarations first, since objects need to be defined in Substance before they're referenced
    // TODO factor out V,E,F code and 3-tuple
    for (let v of mesh.vertices) {
	prog.push(decl(cname, vtype, v.index));
	prog.push(label(cname, vtype, v.index));
	// TODO document these conventions
	penrose_to_mesh[objName(cname, vtype, v.index)] = v.index;
    }

    for (let e of mesh.edges) {
	prog.push(decl(cname, etype, e.index));
	prog.push(label(cname, etype, e.index));
	penrose_to_mesh[objName(cname, etype, e.index)] = e.index;
    }

    for (let f of mesh.faces) {
	prog.push(decl(cname, ftype, f.index));
	prog.push(label(cname, ftype, f.index));
	penrose_to_mesh[objName(cname, ftype, f.index)] = f.index;
    }

    // Build connectivity info
    for (let v of mesh.vertices) {
	// TODO use mkE
	for (let e of v.adjacentEdges()) {
	    prog.push(inVE(cname, v.index, e.index));
	}
    }

    // TODO: check what faces should be in the star
    for (let f of mesh.faces) {
	let edges = [];

	for (let e of f.adjacentEdges()) {
	    edges.push(e.index);
	}

	if (edges.length == 3) {
	    prog.push(mkF(objName(cname, ftype, f.index), 
			  cname, edges[0], edges[1], edges[2]));
	} else {
	    console.log("malformed face: ", f);
	}
    }

    // -------------------------------------------
    // TODO factor this out
    // Select the vertices, edges, and faces that are in the star

    console.log("penrose to mesh", penrose_to_mesh);

    // Note that for a simplex, an object is simply the index
    for (let v of star_sc.vertices) {
	console.log(v);
	prog.push(selected(vtype, objName(cname, vtype, v)));
    }

    for (let e of star_sc.edges) {
	prog.push(selected(etype, objName(cname, etype, e)));
    }

    for (let f of star_sc.faces) {
	prog.push(selected(ftype, objName(cname, ftype, f)));
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
