/* This is a plugin for the mesh domain, meant to be used with mesh-domain/mesh.dsl and mesh-domain/SimplicialComplex.sty.

It does the following:

It sets up a pseudorandom generator for both rand-mesh (via seedrandom replacing Math.random) and mesh-plugin (called `prg`).

First, it instantiates every declared simplicial complex. This is done in `makeObj`. To make a random mesh, it calls rand-mesh.js, using parameters `pointRange` and `numPointsRange`. To make the data structure, it calls geometry-processing-js to make a mesh and its simplicial complex. All objects are created and stored in the `objs` key-value store in `makeSub`, indexed by name. 

It names and refers to objects by the convention defined in `objName`, which relies on the internal index of an object in geometry-processing-js. For example, a vertex belonging to the simplicial complex `K1` might be called `K1_V1`.

Next, it binds and manages names that are declared in the Substance, like `Vertex i`, `Face f`, and `Edge e`. It does this in `makeNameMappings` by selecting a random primitive object (by index) and keeping track of which Substance name is bound to which generated name. It also, later, says that these have been declared in `makeUserLines`. (A primitive object is a vertex, edge, or face.)

Then, it performs simplicial complex operations by calling the corresponding function in geometry-processing-js.
  The supported functions can be found in `doFnCall` and are currently `StarV`, `Closure`, `Link`, `LinkV`, and `Boundary`.
  Because the Substance JSON may not preserve the order of the Substance program, each function call is evaluated by checking if its argument has been created, and if not, recursively evaluating the Substance program until all arguments to the current function call have been created. All results of function calls return objects (mesh subsets and simplical complex operators) as well as any relevant names.

Then, with all the objects that have been made, the plugin outputs Substance code for the meshes and subsets. This is done in `makeProg`. For a simplicial complex, for each vertex, edge, and face in it, it outputs a declaration, In statement, and labeling statement. (It has to build simple connectivity info to create the edge and face value constructors.) For a mesh subset, it outputs In statements for each vertex, edge, and face in it. All statements are output with respect to any names bound in Substance, if they exist, by substituting them for generated names.

Finally, it makes a JSON file for Style that contains the vertex positions of each mesh. This is done in `makeSty`. Note that it builds the geometry of the mesh here by calling geometry-processing-js. (It samples a random mesh, runs a Delaunay triangulation on it, and optimizes it so the triangles are near equilateral.) It also ignores any z-positions of the vertices. 

Some parts are unfinished and the plugin hasn't been completely tested over general Substance programs. See the PR for further documentation. */

const Fs = require('fs');
const _ = require('lodash');
var seedrandom = require('seedrandom');

// Geometry processing imports
const Vertex = require('./geometry-processing-js/node/core/vertex.js');
const Edge = require('./geometry-processing-js/node/core/edge.js');
const Halfedge = require('./geometry-processing-js/node/core/halfedge.js');
const MeshSubset = require('./geometry-processing-js/node/core/mesh-subset.js');
const Mesh = require('./geometry-processing-js/node/core/mesh.js');
const Geometry = require('./geometry-processing-js/node/core/geometry.js');
const MeshIO = require('./geometry-processing-js/node/utils/meshio.js');
const SC = require('./geometry-processing-js/node/projects/simplicial-complex-operators/simplicial-complex-operators.js');

// Personal library imports
const OptMesh = require('./optimize-mesh.js');
const RandMesh = require('./rand-mesh.js');

// Input meshes
const TetraMesh = require('./input/tetrahedron.js');
const SquareMesh = require('./input/square.js');
const TriangleMesh = require('./input/triangle.js');
const InnerMesh = require('./input/inner.js');

const global_mesh = undefined;
const newline = "\n";
const vtype = "Vertex";
const etype = "Edge";
const ftype = "Face";

const pointRange = [-1, 1]; // Range to sample mesh points from (geometry)
const numPointsRange = [6, 10];

var seed0; // MUTABLE: gets set by random seed from Style and is reused in rand-mesh
var prg; // MUTABLE (used in lieu of Math.Random which is globally set in rand-mesh, so we can control both the vertex selection and the number of them)

const crash = () => { console.log("crashing"); console.log(null[0]); }

const isInPred = s => s === "InVS" || s === "InES" || s === "InFS";
// doesn't check that the type matches pred type--presumably the substance typechecker dealt with that

const isPrimitiveType = t => t === "Vertex" || t === "Edge" || t === "Face";
// i.e. is not a composite object (mesh subset or simplicial complex)

function objName(cname, objType, index) {
    return cname + "_" + tstr(objType) + index;
}

function userSpecified(objName, objType) {
    return "Declared" + tstr(objType) + "(" + objName + ")";
}

// TODO: assumes all simplicial complices have name format [single character][numbers]
function labelSC(cname) {
    return "Label " + cname + " $" + cname[0] + "_{" + cname.substring(1) + "}$";
}

function label(cname, objName, objType, index) {
    return "Label " + objName + " $" 
    // e.g. K_1 V_4
	// + cname[0] + "_{" + cname.substring(1) + "} "
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

// `subset` should be a MeshSubset object from gp.js
// `prim` should have schema like this: { scName: 'K', type: 'Face', assignedName: 'K_F2', index: 2 }
// NOTE: mutates the input subset; does not return
function addPrimitiveToSubset(subset, prim) {
    if (prim.type === "Vertex") {
	subset.addVertex(prim.index);
    } else if (prim.type === "Edge") {
	subset.addEdge(prim.index);
    } else if (prim.type === "Face") {
	subset.addFace(prim.index);
    } else {
	console.error("Unsupported type to add to MeshSubset", prim.type);
	crash();
    }
}

function makeSComplex(cname) {
    const numPts = Math.floor(RandMesh.sampleFrom(numPointsRange));
    console.log("num pts", numPts);
    let mesh_input = RandMesh.makeRandMesh(numPts, pointRange);
    console.log("mesh input", mesh_input);

    let polygonSoup = MeshIO.readOBJ(mesh_input);
    let mesh = new Mesh.Mesh();
    mesh.build(polygonSoup);
    let v1 = mesh.vertices.length;

    // Note: the positions are that of a random mesh with a Delaunay triangulation. The positions do not affect the connectivity, so we ignore the positions until we need to to output it for the Style plugin.

    // Construct a simplicial complex for a mesh
    // NOTE: this changes the indices of the mesh
    let SCO = new SC.SimplicialComplexOperators(mesh);
    let v2 = mesh.vertices.length;

    if (v1 !== v2) { throw new Error("number of vertices changed after SCO"); }

    return { type: 'SimplicialComplex',
	     name: cname,
	     mesh: mesh,
	     obj_file: mesh_input,
	     sc: SCO
	   };
}

// Expects a mesh made by makeSComplex
function doStarV(stmt, nameMappings, scObj) {
    let starName = stmt.varName;
    let vertexName = stmt.fargNames[0];
    // Take the star at a specific vertex
    let cname = scObj.name;
    let SCO = scObj.sc;
    let vertexIndex = nameMappings.sub2plugin[vertexName].index;

    let selectedSimplices = new MeshSubset();
    selectedSimplices.addVertices([vertexIndex]);
    selectedSimplices.addEdges([]);
    selectedSimplices.addFaces([]);

    let star_sc = SCO.star(selectedSimplices);

    // Mesh subset object
    return { type: 'MeshSubset',
	     name: starName, // name bound in Substance
	     scName: cname,
	     sc: SCO, // Has to carry the simplicial complex operators with it...
	     // selectedSimplices: selectedSimplices,
	     meshSubset: star_sc,
	   };
}

function doStar(stmt, subsetObj) {
    let subcomplexName = stmt.varName;
    let cname = subsetObj.scName;
    let SCO = subsetObj.sc;

    let meshSubset = subsetObj.meshSubset;
    let star_ms = SCO.star(meshSubset);

    return { type: 'MeshSubset',
	     name: subcomplexName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: star_ms
	   };
}

function doClosure(stmt, subsetObj) {
    let subcomplexName = stmt.varName;
    let cname = subsetObj.scName;
    let SCO = subsetObj.sc;

    let meshSubset = subsetObj.meshSubset;
    // console.log(stmt, subsetObj);

    let closure_ms = SCO.closure(meshSubset);

    return { type: 'MeshSubset',
	     name: subcomplexName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: closure_ms
	   };
}

function doClosureV(stmt, nameMappings, scObj) {
    let subsetName = stmt.varName;
    console.log("do closure v", scObj);

    let cname = scObj.name;
    let SCO = scObj.sc;

    let vertexName = stmt.fargNames[0];
    let vertexIndex = nameMappings.sub2plugin[vertexName].index;

    let selectedSimplices = new MeshSubset();
    selectedSimplices.addVertices([vertexIndex]);
    selectedSimplices.addEdges([]);
    selectedSimplices.addFaces([]);

    let link_ms = SCO.closure(selectedSimplices);

    return { type: 'MeshSubset',
	     name: subsetName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: link_ms
	   };
}

// TODO: factor out this boilerplace
function doLink(stmt, subsetObj) {
    let subsetName = stmt.varName;
    let cname = subsetObj.scName;
    let SCO = subsetObj.sc;

    let meshSubset = subsetObj.meshSubset;
    let link_ms = SCO.link(meshSubset);

    return { type: 'MeshSubset',
	     name: subsetName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: link_ms
	   };
}

function doLinkV(stmt, nameMappings, scObj) {
    let subsetName = stmt.varName;
    console.log("do link v", scObj);

    let cname = scObj.name;
    let SCO = scObj.sc;

    let vertexName = stmt.fargNames[0];
    let vertexIndex = nameMappings.sub2plugin[vertexName].index;

    let selectedSimplices = new MeshSubset();
    selectedSimplices.addVertices([vertexIndex]);
    selectedSimplices.addEdges([]);
    selectedSimplices.addFaces([]);

    let link_ms = SCO.link(selectedSimplices);

    return { type: 'MeshSubset',
	     name: subsetName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: link_ms
	   };
}

function doBoundary(stmt, subsetObj) {
    let subcomplexName = stmt.varName;
    let cname = subsetObj.scName;
    let SCO = subsetObj.sc;

    let meshSubset = subsetObj.meshSubset;
    let boundary_ms = SCO.boundary(meshSubset);

    return { type: 'MeshSubset',
	     name: subcomplexName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: boundary_ms
	   };
}

function doSetMinus(stmt, o1, o2) {
    console.log("doSetMinus", stmt, o1, o2);
    let subcomplexName = stmt.varName;

    let cname1 = o1.scName;
    let SCO1 = o1.sc;
    let meshSubset1 = o1.meshSubset;

    // let cname2 = o2.scName;
    // let SCO2 = o2.sc;
    let meshSubset2 = o2.meshSubset;

    let meshSubset3 = MeshSubset.deepCopy(meshSubset1);

    // console.log("meshSubset 1,2,3 before setminus", meshSubset1, meshSubset2, meshSubset3);
    // `delete` mutates it. So you need to copy
    meshSubset3.deleteSubset(meshSubset2);

    // console.log("meshSubset 1,2,3 after setminus", meshSubset1, meshSubset2, meshSubset3);

    // TODO: Does it matter which scName and sc I attach?
    return { type: 'MeshSubset',
	     name: subcomplexName,
	     scName: cname1, // TODO: OK to use this?
	     sc: SCO1, // TODO: OK to use this?
	     meshSubset: meshSubset3
	   };
}

function doUnion(stmt, o1, o2, json, objs) {
    console.log("doUnion", stmt, o1, o2);

    let subcomplexName = stmt.varName;

    if (o1.type === "MeshSubset" && o2.type === "MeshSubset") {
	let cname1 = o1.scName;
	let SCO1 = o1.sc;
	let meshSubset1 = o1.meshSubset;
	let meshSubset2 = o2.meshSubset;
	let meshSubset3 = MeshSubset.deepCopy(meshSubset1);
	meshSubset3.addSubset(meshSubset2);

	// TODO: Does it matter which scName and sc I attach?
	return { type: 'MeshSubset',
		 name: subcomplexName,
		 scName: cname1, // TODO: OK to use this?
		 sc: SCO1, // TODO: OK to use this?
		 meshSubset: meshSubset3
	       };
    } else if (isPrimitiveType(o1.type) && isPrimitiveType(o2.type)) {
	let argObj1 = findMesh(stmt.fargNames[0], json, objs);
	let argObj2 = findMesh(stmt.fargNames[1], json, objs);
	let cname1 = o1.scName;
	let SCO1 = argObj1.sc;	

	let subsetUnion = new MeshSubset();
	addPrimitiveToSubset(subsetUnion, o1);
	addPrimitiveToSubset(subsetUnion, o2);
	// console.log("subsetUnion", subsetUnion);

	// TODO: Does it matter which scName and sc I attach?
	return { type: 'MeshSubset',
		 name: subcomplexName,
		 scName: cname1, // TODO: OK to use this?
		 sc: SCO1, // TODO: OK to use this?
		 meshSubset: subsetUnion
	       };
    } else {
	console.error("Unsupported types for Union", o1.type, o2.type);
	crash();
    }
}

// Return the Substance programs
function scToSub(mappings, scObj) {
    let mesh = scObj.mesh;
    let cname = scObj.name;
    let prog = [];
    let plugin2sub = mappings.plugin2sub;
    let remappedNames = Object.keys(plugin2sub); // Any plugin names that have been bound in Substance by the end user, like K1_V2

    console.log("remapped names", remappedNames);

    // SC names can't be remapped
    prog.push(labelSC(cname));

    // TODO: the declarations should really be happening in a separate function call before this...
    // Emit all declarations first, since objects need to be defined in Substance before they're referenced
    // TODO factor out V,E,F code and 3-tuple
    for (let v of mesh.vertices) {
	let vname = objName(cname, vtype, v.index);

	if (!wasRemapped(plugin2sub, vname)) {
	    prog.push(decl(vtype, vname));
	    prog.push(inSubset(vtype, vname, cname));
	    prog.push(label(cname, vname, vtype, v.index));
	}
    }

    for (let e of mesh.edges) {
	let ename = objName(cname, etype, e.index);

	if (!wasRemapped(plugin2sub, ename)) {
	    prog.push(decl(etype, ename));
	    prog.push(inSubset(etype, ename, cname));
	    prog.push(label(cname, ename, etype, e.index));
	}
    }

    for (let f of mesh.faces) {
	let fname = objName(cname, ftype, f.index);

	if (!wasRemapped(plugin2sub, fname)) {
	    prog.push(decl(ftype, fname));
	    prog.push(inSubset(ftype, fname, cname));
	    prog.push(label(cname, fname, ftype, f.index));
	}
    }

    // Build connectivity info for edges and faces
    // Substitute any renamed objects, if they exist
    for (let e of mesh.edges) {
	let h = e.halfedge;
	let v1 = h.vertex;
	let v2 = h.twin.vertex;

	let ename = substitute(plugin2sub, objName(cname, etype, e.index));
	let vname1 = substitute(plugin2sub, objName(cname, vtype, v1.index));
	let vname2 = substitute(plugin2sub, objName(cname, vtype, v2.index));
	
	prog.push(mkE(ename, vname1, vname2));
    }

    // TODO: check what faces should be in the star
    for (let f of mesh.faces) {
	let edges = [];

	for (let e of f.adjacentEdges()) {
	    edges.push(e.index);
	}

	let enames = edges.map(i => substitute(plugin2sub, objName(cname, etype, i)));

	if (enames.length == 3) {
	    let fname = substitute(plugin2sub, objName(cname, ftype, f.index));
	    prog.push(mkF(fname, cname, enames[0], enames[1], enames[2]));
	} else {
	    console.log("malformed face: ", f);
	    crash();
	}
    }

    return prog;
}

function subsetToSub(mappings, subsetWrapper) {
    console.log("subset to sub", subsetWrapper);
    let cname = subsetWrapper.scName;
    let sname = subsetWrapper.name;
    let star_sc = subsetWrapper.meshSubset;
    let plugin2sub = mappings.plugin2sub;

    let prog = [];

    // Specify the vertices, edges, and faces that are in a particular mesh subset
    // Note that for a simplex, an object is simply the index
    for (let v of star_sc.vertices) {
	console.log("selected vertex: ", v);
	prog.push(inSubset(vtype, substitute(plugin2sub, objName(cname, vtype, v)), sname));
    }

    for (let e of star_sc.edges) {
	prog.push(inSubset(etype, substitute(plugin2sub, objName(cname, etype, e)), sname));
    }

    for (let f of star_sc.faces) {
	prog.push(inSubset(ftype, substitute(plugin2sub, objName(cname, ftype, f)), sname));
    }

    return prog;
}

// find the mesh object that the vertex/edge/face belongs to (if any)
// should be declared with an InVS/InFS/InES statement in substance, assuming a valid substance program with parg order preserved
function findMesh(name, json, objs) {
    // first find the name of the mesh, then look it up in the objects we made

    // TODO: write a "find" function
    let meshNames = json.constraints.predicates
	.filter(o => isInPred(o.pname) && name === o.pargs[0].Left)
        .map(o => o.pargs[1].Left);

    if (meshNames.length !== 1) {
	console.log("expected to find vertex/edge/face " + name + " in exactly one mesh, but found " + meshNames.length);
	crash();
    }

    console.log("found mesh: " + meshNames[0]);
    let meshName = meshNames[0];

    return objs[meshName];
}

function findSubset(subsetName, subsets) {
    return subsets.filter(o => o.name === subsetName)[0];
}

function makeNameMappings(json, objs) {
    // Deal with named vertex, edge, and face statements. For example (just for vertices):
    // Choose a random vertex in the mesh (fail if no vertices)
    // Make a mapping from Substance name ("v") to that vertex's index, simplicial complex, and generated name ("K1_v1")
    // What info do we need about "v"? Do we ever need a reverse mapping?
    // In Substance, we might say "SelectedV(v)" or "S := Star(v)" (no MkEdge statements allowed) or "Label v $...$"
    // That means this plugin needs to
    // 1. generate Star statements WRT the correct vertex name, but respecting the connectivity of the actual vertex it maps to
    // 2. NOT declare that Vertex or Label it, but do output the In edge statement (and anything relating to its connectivity)

    // This only deals with Vertex v in S (where SimplicialComplex S), not other kinds of objects in non-sc objects

    // Mappings from Substance name to subpart of structure (name assigned in code here), as well as the reverse mapping
    let subToAssignedMapping = {};
    let assignedToSubMapping = {};

    let objMappings = json.objects
	.filter(o => isPrimitiveType(o.objType)); // [ { objName : String, objType : String } ]

    for (let o of objMappings) {
	let oname = o.objName;
	let otype = o.objType;
	let scObj = findMesh(oname, json, objs);
	let mesh = scObj.mesh;
	let scName = scObj.name;

	subToAssignedMapping[oname] = { scName: scName, // Should this be a pointer to the SC?
					type: otype,
					assignedName: undefined,
					index: undefined };

	let objList; // List of objects to choose from

	if (otype === "Vertex") { objList = mesh.vertices; }
	else if (otype === "Edge") { objList = mesh.edges; }
	else if (otype === "Face") { objList = mesh.faces; }
	else { console.error("Type", otype, "not found"); crash(); }

	if (objList.length == 0) {
	    console.log("error: mesh has no objects of type", otype, "!");
	} else {
	    let oi = Math.floor(prg() * objList.length); // NOT Math.random (which is now a PRG), but a PRG with a separate seed
	    let selectedObj = objList[oi];
	    let assignedName = objName(scName, otype, oi); // this is a name generated by the plugin, such as `K1_V0`

	    subToAssignedMapping[oname].assignedName = assignedName;
	    subToAssignedMapping[oname].index = oi;

	    assignedToSubMapping[assignedName] = oname;
	}
    }

    console.log("name mappings, sub2plugin", subToAssignedMapping);

    return { sub2plugin: subToAssignedMapping,
	     plugin2sub: assignedToSubMapping };
}

// Given plugin2sub and a plugin name Ki_Vi, return the sub name `v` if it exists. Otherwise return the plugin name.
function substitute(plugin2sub, pluginName) {
    let remappedNames = Object.keys(plugin2sub);
    return remappedNames.includes(pluginName) ? plugin2sub[pluginName] : pluginName;
}

function wasRemapped(plugin2sub, pluginName) {
    let remappedNames = Object.keys(plugin2sub);
    return remappedNames.includes(pluginName);
}

function makeUserLines(mappings) {
    let prog = [];
    let sub2plugin = mappings.sub2plugin;

    // Note the things that were user-specified for Style to match on (e.g. `DeclaredV(i)`)
    for (let subName of Object.keys(sub2plugin)) {
	let objType = sub2plugin[subName].type;
	prog.push(userSpecified(subName, objType));
    }

    return prog;
}

// Examine an object and call the right Sub prog generating function
function makeProg(mappings, o) {
    console.log("make prog o", o);

    if (o.type === 'SimplicialComplex') {
	return scToSub(mappings, o);
    } else if (o.type === 'MeshSubset') {
	return subsetToSub(mappings, o);
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

function lookupBoundvar(name, fnCalls) {
    let res = fnCalls.filter(o => o.varName === name);
    if (res.length < 1) { return undefined; }
    return res[0];
}

function doFnCall(json, objs, mappings, fnCall) { // TODO factor out mappings
    let bindVar = fnCall.varName;
    let fname = fnCall.fname;
    let fnArgs = fnCall.fargNames;
    let res = undefined;

    console.log("objs in doFnCall", objs);

    // If function call has already been done, move on
    // (NOTE: assuming you aren't binding twice like y1 := f(x), y1 := g(z))
    if (objs[bindVar]) { return objs; }

    // Otherwise, do the function call, store the value, and return the new values
    // First, if an argument to a function call is not yet computed, do it and memoize it in objs
        // (keeps updating it -- should this be a fold?)
    for (let fnArg of fnArgs) {
	console.log("fn arg", fnArg, objs[fnArg]);

	// It should be a bound variable in functions, otherwise the Substance program is invalid
	if (!objs[fnArg]) {
	    let bvar_fncall = lookupBoundvar(fnArg, json.constraints.functions);

	    if (!bvar_fncall) {
		console.log("error: function argument", fnArg, "not declared or bound");
		return objs; // skip
	    }
	    let argsComputed = doFnCall(json, objs, mappings, bvar_fncall); // could update multiple objects... is this pass by reference or by value?
	    objs = Object.assign(objs, argsComputed); // TODO: use argsComputed? or merge?
	}
    }

    // Do the fn call based on the fn's name
    // Do different things based on the type of argument (since Domain program uses subtyping)
    // There's probably a more generic way to do this based on the Element type and geometry-processing.js types of the function

    let argName0 = fnArgs[0];
    let argObj0 = objs[argName0];
    let argType0 = argObj0.type;
    console.log("function call", fname, "with arg name 0", argName0, "of type", argType0);

    if (fname === "Star") {
	if (argType0 === "MeshSubset") {
	    let argObj = argObj0;
	    res = doStar(fnCall, argObj);
	} else if (argType0 === "Vertex") {
	    let argObj = findMesh(argName0, json, objs);
	    res = doStarV(fnCall, mappings, argObj);
	} else {
	    console.error("Unknown type", argType0, "for call to `Star`; crash");
	    res = undefined;
	    crash();
	}
    } else if (fname === "Closure") {
	if (argType0 === "MeshSubset") {
	    let argObj = argObj0;
	    res = doClosure(fnCall, argObj);
	} else if (argType0 === "Vertex") {
	    let argObj = findMesh(argName0, json, objs);
	    res = doClosureV(fnCall, mappings, argObj);
	} else {
	    console.error("Unknown type", argType0, "for call to `Closure`; crash");
	    res = undefined;
	    crash();
	}
    } else if (fname === "Link") {
	if (argType0 === "MeshSubset") {
	    let argObj = argObj0;
	    res = doLink(fnCall, argObj);
	} else if (argType0 === "Vertex") {
	    let argObj = findMesh(argName0, json, objs);
	    res = doLinkV(fnCall, mappings, argObj);
	} else {
	    console.error("Unknown type", argType0, "for call to `Link`; crash");
	    res = undefined;
	    crash();
	}
    } else if (fname === "Boundary") { // Supposed to only operate on a SComplex; here we look for a mesh subset
	res = doBoundary(fnCall, argObj0);
    } else if (fname === "SetMinus") {
	let argName1 = fnArgs[1];
	let argObj1 = objs[argName1];
	let argType1 = argObj1.type;
	console.log("function call", fname, "with arg name 1", argName1, "of type", argType1);

	res = doSetMinus(fnCall, argObj0, argObj1);
    } else if (fname === "Union") {
	let argName1 = fnArgs[1];
	let argObj1 = objs[argName1];
	let argType1 = argObj1.type;
	console.log("function call", fname, "with arg name 1", argName1, "of type", argType1);

	res = doUnion(fnCall, argObj0, argObj1, json, objs);
    } else {
	console.log("unimplemented function", fname);
	crash();
    }

    if (res) { objs[bindVar] = res; }

    return objs; // returns the whole updated map. but maybe it shouldn't return anything?
}

// Make the values for style, using just the object map made while processing the Substance program
function makeSty(objs, plugin2sub) {
    // Output the vertex positions of each mesh
    let scs = objs.filter(o => o.type === "SimplicialComplex");

    let vals = [];
    for (let sc of scs) {
	let mesh = sc.mesh;
	let cname = sc.name;

	let polygonSoup = MeshIO.readOBJ(sc.obj_file);
	// Note: geometry is rebuilt after SCO is built, so mesh indices are accurate
	let geometry = new Geometry.Geometry(mesh, polygonSoup["v"], false);
	let positions = geometry.positions;
	console.log("positions", positions);
	console.log("positions length", Object.keys(positions).length);
	console.log("vertices length", mesh.vertices.length);

	// Optimize the vertex positions of the mesh so each triangle looks close to equilateral.
	console.log("sending mesh to be optimized");
	let optimizedPositions = OptMesh.optimizeMesh(mesh, geometry, positions);
	    console.log("optimized positions", optimizedPositions);

	for (let v of mesh.vertices) {
	    let vi = v.index;
	    let vname = substitute(plugin2sub, objName(cname, vtype, vi));
	    let vpos = optimizedPositions[v]; // Vector object, throw away z pos

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
    // let subPreds = json.constraints.predicates;
    let subFnCalls = json.constraints.functions;

    let objs_flat = subDecls.map(decl => makeObj(decl))
	.filter(o => o !== undefined);

    let objs = [];
    for (let obj of objs_flat) {
	objs[obj.name] = obj; // Convert to a key-value store
    }

    // Deal with statements that select a subpart of a whole (TODO: currently just a vertex of an SC)
    let mappings = makeNameMappings(json, objs);

    // Merge key-value pairs
    const objsWithMappings = Object.assign(objs, mappings.sub2plugin);
    console.log("objs with mappings", objsWithMappings);

    // Fold over the function calls, updating objs whenever new ones are made, and recursively performing function calls to make any objs that haven't been made
    let finalObjs = subFnCalls.reduce((objs_acc, fnCall) => doFnCall(json, objs_acc, mappings, fnCall),
				   objs);

    let objs_flat_final = [];
    for (var objName in finalObjs) { // Convert from key-value store back to flat list of objects
	objs_flat_final.push(finalObjs[objName]);
    }

    // Output Substance code
    console.log("final objs:", finalObjs, objs_flat_final);
    let subUserLines = makeUserLines(mappings);
    let lines = _.flatten(objs_flat_final.map(o => makeProg(mappings, o)));
    let allLines = subUserLines.concat(lines)
    let subProgStr = allLines.join(newline);

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

    let seeds = subJSON.params.map(x => x.contents.contents);
    console.log("seeds", seeds);
    seed0 = seeds[0];
    // Use a prg here and set Math.random globally in rand-mesh so we can have different seeds controlling different aspects (vertex choice or mesh topology)
    prg = seedrandom(seed0);
    RandMesh.setUpRand(seeds[1]);
    console.log("Math.rand in plugin", Math.random());

    let results = makeSubAndValues(subJSON.substance);
    let [newSub, styVals] = [results.newSub, results.styVals];

    console.log("writing Substance program: ", newSub);
    Fs.writeFileSync('Sub_instantiated.sub', newSub);

    console.log("writing values for Style: ", styVals);
    Fs.writeFileSync('values.json', styVals);

    console.log("ending mesh plugin")
}

main();

module.exports = {main, makeSubAndValues, global_mesh};
