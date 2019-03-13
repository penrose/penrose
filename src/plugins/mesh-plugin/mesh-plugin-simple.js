const Fs = require('fs');
const _ = require('lodash');

// Geometry processing imports
const Vertex = require('./geometry-processing-js/node/core/vertex.js');
const Edge = require('./geometry-processing-js/node/core/edge.js');
const Halfedge = require('./geometry-processing-js/node/core/halfedge.js');
const MeshSubset = require('./geometry-processing-js/node/core/mesh-subset.js');
const Mesh = require('./geometry-processing-js/node/core/mesh.js');
const Geometry = require('./geometry-processing-js/node/core/geometry.js');
const MeshIO = require('./geometry-processing-js/node/utils/meshio.js');

const SC = require('./geometry-processing-js/node/projects/simplicial-complex-operators/simplicial-complex-operators.js');

// Input meshes
// const FaceMesh = require('./geometry-processing-js/input/face.js');
// const HexMesh = require('./geometry-processing-js/input/hexagon.js');
// const DiskMesh = require('./geometry-processing-js/input/small_disk.js');
const TetraMesh = require('./input/tetrahedron.js');
const SquareMesh = require('./input/square.js');
const TriangleMesh = require('./input/triangle.js');
const TriangleMesh3 = require('./input/triangle-3.js');

const mesh_to_use = SquareMesh;

global_mesh = undefined;
newline = "\n";
vtype = "Vertex";
etype = "Edge";
ftype = "Face";

function objName(cname, objType, index) {
    return cname + "_" + tstr(objType) + index;
}

function label(cname, objName, objType, index) {
    return "Label " + objName
	+ " $" + cname[0] + "_{" + cname.substring(1) + "} "
	+ tstr(objType) + "_{" + index + "}$";

}

function decl(objType, oname) {
    return objType + " " + oname;
}

function inVE(vname, ename) {
    return "InVE(" + vname + ", " + ename + ")";
}

// TODO: factor out subset?
function inSubset(objType, oname, sname) {
    return "In" + tstr(objType) + "S(" + oname + ", " + sname + ")";
}

function mkE(ename, vname1, vname2) {
    return ename + " := MkEdge(" + vname1 + ", " + vname2 + ")";
}

function mkF(fname, cname, ename1, ename2, ename3) {
    return fname + " := MkFace(" + ename1 + ", " + ename2 + ", " + ename3 + ")";
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
    // TODO: make a simple random mesh, not a constant one
    // Tetrahedron has vertices indexed from 0-3 (inclusive), edges 0-5, faces 0-3
    // Maybe I should draw a 2D mesh
    let polygonSoup = MeshIO.readOBJ(mesh_to_use);
    let mesh = new Mesh.Mesh();
    mesh.build(polygonSoup);
    // TODO get rid of this
    let geometry = new Geometry.Geometry(mesh, polygonSoup["v"], false);

    // Construct a simplicial complex for a mesh
    let SCO = new SC.SimplicialComplexOperators(mesh);

    global_mesh = { polygonSoup, mesh, geometry };

    return { type: 'SimplicialComplex',
	     name: cname,
	     mesh: mesh,
	     geometry: geometry
	     // sc: SCO
	   };
}

// Return the Substance programs
function scToSub(mappings, scObj) {
    let mesh = scObj.mesh;
    let cname = scObj.name;
    let prog = [];
    let plugin2sub = mappings.plugin2sub;
    let remappedNames = Object.keys(plugin2sub); // Any plugin names that have been bound in Substance by the end user, like K1_V2

    console.log("remapped names", remappedNames);

    // Emit all declarations first, since objects need to be defined in Substance before they're referenced

    for (let vi = 0; vi < mesh.vertices.length; vi++) {
	let v = mesh.vertices[vi];
	let vname = objName(cname, vtype, v.index);

	prog.push(decl(vtype, vname));
	prog.push(inSubset(vtype, vname, cname));
	// prog.push(label(cname, vname, vtype, v.index));
    }

    for (let ei = 0; ei < mesh.edges.length; ei++) {
	let e = mesh.edges[ei];
	let ename = objName(cname, etype, e.index);

	prog.push(decl(etype, ename));
	// prog.push(label(cname, ename, etype, e.index));
    }

    for (let f of mesh.faces) {
	let fname = objName(cname, ftype, f.index);

	prog.push(decl(ftype, fname));
	// prog.push(label(cname, fname, ftype, f.index));
    }

    // TODO: does this work for things on the boundary?

    // Build connectivity info for edges and faces

    // Find the two vertices for an edge
    for (let e of mesh.edges) {
	let h = e.halfedge;
	let v1 = h.vertex;
	let v2 = h.twin.vertex;

	console.log("----------------");
	console.log("e", e);
	console.log("v1", v1);
	console.log("v2", v2);

	let ename = objName(cname, etype, e.index);
	let vname1 = objName(cname, vtype, v1.index);
	let vname2 = objName(cname, vtype, v2.index);
	
	prog.push(mkE(ename, vname1, vname2));
    }

    // Find the three edges for a face
    for (let f of mesh.faces) {
	let edges = [];

	for (let e of f.adjacentEdges()) {
	    edges.push(e.index);
	}

	let enames = edges.map(i => objName(cname, etype, i));

	if (enames.length == 3) {
	    prog.push(mkF(objName(cname, ftype, f.index),
			  cname, enames[0], enames[1], enames[2]));
	} else {
	    console.log("malformed face: ", f);
	}
    }

    // (Testing) find the three vertices for a face
    // since this is what's used to build the three.js mesh
    // The names of the vertices are not changing over different runs (e.g. K0_V0 (seems to) always have position (0,0), etc.)
    // So why is the face connectivity seeming to change??
    // Can it have something to do with how I call the mesh constructor in node?
/*
    for (let f of mesh.faces) {
	// F0 should always be on the 0-indexed vertices (0,1,2) as specified in the obj file, right???
	let vs = [];

	for (let v of f.adjacentVertices()) {
	    vs.push(v.index);
	}

	let vnames = vs.map(i => objName(cname, vtype, i));

	console.log("f index:", f.index);
	console.log("f's adjacent vertices:", f.adjacentVertices());
	console.log("vs:", vs);
	console.log("vnames:", vnames);

	if (vnames.length == 3) {
	    prog.push(mkF(objName(cname, ftype, f.index),
			  cname, vnames[0], vnames[1], vnames[2]));
	} else {
	    console.log("malformed face: ", f);
	}
    } */

    return prog;
}

// Examine an object and call the right Sub prog generating function
function makeProg(mappings, o) {
    console.log("make prog o", o);

    if (o.type === 'SimplicialComplex') {
	return scToSub(mappings, o);
    }
}

function makeObj(decl) {
    // Dispatch based on stype.
    // SimplicialComplex: make one (these can never result from fn calls)
    // Vertex, Edge, Face: select a subpart of a simplicial complex (or simplicial subset, or subcomplex)--not handled here
    // SSubset, SComplex: ignore? (these always need to result from fn calls)
    let stype = decl.objType;
    let sname = decl.objName;
    let res = undefined;

    if (stype === "SComplex") { res = makeSComplex(sname); }
    return res;
}

// Make the values for style, using just the object map made while processing the Substance program
function makeSty(objs, plugin2sub) {
    // Output the vertex positions of each mesh
    let scs = objs.filter(o => o.type === "SimplicialComplex");

    let vals = [];
    for (let sc of scs) {
	let mesh = sc.mesh;
	let cname = sc.name;

	let polygonSoup = MeshIO.readOBJ(mesh_to_use);
	let new_geometry = new Geometry.Geometry(mesh, polygonSoup["v"], false);

	let positions = new_geometry.positions; // NOT sc.geometry
	console.log("positions", positions);

	for (let v of mesh.vertices) {
	    let vi = v.index;
	    let vname = objName(cname, vtype, vi);
	    let vpos = positions[v]; // Vector object, throw away z pos; access by string of index

	    let pos_json_x = { propertyName: "x",
			       propertyVal: vpos.x };
	    let pos_json_y = { propertyName: "y",
			       propertyVal: vpos.y };
	    let local_positions = [pos_json_x, pos_json_y];

	    let local_json = {};
	    local_json["subName"] = vname;
	    local_json["nameVals"] = local_positions;
	    vals.push(local_json);
	}
    }

    console.log("mesh vertices", scs[0].mesh.vertices);

    return JSON.stringify(vals);
}

function makeSub(json) {
   // TODO: recursion limit
    // T x: For each object that needs to be constructed: map `T` to the corresponding function here; make `x` and add it to the list of objects
    // y := f([x]): For each function call: map `f` to the corresponding function here and look up the `x` in the list of objects
      // If the `x` exists, find it and pass it in as an argument
      // Otherwise, make `x`, add it to the list of objects, and pass it in as an argument [i.e. memoized]
      // Bind the resulting object to `y` and add it to the list of objects
    // P([x]): ignore predicates for now

    let subDecls = json.objects;

    let objs_flat = subDecls.map(decl => makeObj(decl))
	.filter(o => o !== undefined);

    // Deal with statements that select a subpart of a whole (TODO: currently just a vertex of an SC)
    let mappings = { sub2plugin: {}, plugin2sub: {} };

    // Output Substance code
    let lines = _.flatten(objs_flat.map(o => makeProg(mappings, o)));
    let subProgStr = lines.join(newline);

    return { objs: objs_flat,
	     subProgStr: subProgStr,
	     plugin2sub: mappings.plugin2sub
	   };
}

function makeSubAndValues(json) {
    let res = makeSub(json);
    let [objs, subProgStr, plugin2sub] = [res.objs, res.subProgStr, res.plugin2sub];

    return { newSub: subProgStr,
	     styVals: makeSty(objs, plugin2sub)
	   };

}

function main() {
    console.log("starting mesh plugin");

    let rawdata = Fs.readFileSync('Sub_enduser.json');
    let subJSON = JSON.parse(rawdata);
    console.log("Received JSON", subJSON);
    console.log("Received JSON", subJSON.constraints);

    let results = makeSubAndValues(subJSON);
    let [newSub, styVals] = [results.newSub, results.styVals];

    console.log("\nwriting Substance program\n", newSub);
    Fs.writeFileSync('Sub_instantiated.sub', newSub);

    console.log("\nwriting values for Style\n", styVals);
    Fs.writeFileSync('values.json', styVals);

    console.log("ending mesh plugin")
}

main();

module.exports = {main, makeSubAndValues, global_mesh};
