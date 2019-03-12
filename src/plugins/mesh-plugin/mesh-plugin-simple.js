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
    let polygonSoup = MeshIO.readOBJ(SquareMesh);
    let mesh = new Mesh.Mesh();
    mesh.build(polygonSoup);
    let geometry = new Geometry.Geometry(mesh, polygonSoup["v"], false);

    // Construct a simplicial complex for a mesh
    let SCO = new SC.SimplicialComplexOperators(mesh);

    global_mesh = { polygonSoup, mesh, geometry };

    return { type: 'SimplicialComplex',
	     name: cname,
	     mesh: mesh,
	     geometry: geometry,
	     sc: SCO
	   };
}

// Expects a mesh made by makeSComplex
function doStar(stmt, nameMappings, scObj) {
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

function doClosure(stmt, nameMappings, subsetObj) {
    let subcomplexName = stmt.varName;
    let cname = subsetObj.scName;
    let SCO = subsetObj.sc;

    let meshSubset = subsetObj.meshSubset;
    let closure_ms = SCO.closure(meshSubset);

    return { type: 'MeshSubset',
	     name: subcomplexName,
	     scName: cname,
	     sc: SCO,
	     meshSubset: closure_ms
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

function doBoundary(stmt, nameMappings, subsetObj) {
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

// Return the Substance programs
function scToSub(mappings, scObj) {
    let mesh = scObj.mesh;
    let cname = scObj.name;
    let prog = [];
    let plugin2sub = mappings.plugin2sub;
    let remappedNames = Object.keys(plugin2sub); // Any plugin names that have been bound in Substance by the end user, like K1_V2

    console.log("remapped names", remappedNames);

    // Emit all declarations first, since objects need to be defined in Substance before they're referenced
    // TODO factor out V,E,F code and 3-tuple
    for (let v of mesh.vertices) {
	let vname = objName(cname, vtype, v.index);

	prog.push(decl(vtype, vname));
	prog.push(inSubset(vtype, vname, cname));
	prog.push(label(cname, vname, vtype, v.index));
    }

    for (let e of mesh.edges) {
	let ename = objName(cname, etype, e.index);

	prog.push(decl(etype, ename));
	prog.push(label(cname, ename, etype, e.index));
    }

    for (let f of mesh.faces) {
	let fname = objName(cname, ftype, f.index);

	prog.push(decl(ftype, fname));
	prog.push(label(cname, fname, ftype, f.index));
    }

    // TODO: does this work for things on the boundary?

    // Build connectivity info for edges and faces

    // Find the two vertices for an edge
    for (let e of mesh.edges) {
	let h = e.halfedge;
	let v1 = h.vertex;
	let v2 = h.twin.vertex;

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

// find the mesh object that the vertex belongs to (if any)
// should be declared with an InVS statement in substance, assuming a valid substance program with parg order preserved
function findMesh(vname, json, objs) {
    // first find the name of the mesh, then look it up in the objects we made

    // TODO: write a "find" function
    let meshNames = json.constraints.predicates
	.filter(o => o.pname === "InVS" && vname === o.pargNames[0])
        .map(o => o.pargNames[1]);

    if (meshNames.length !== 1) {
	console.log("expected to find vertex " + vname + " in exactly one mesh, but found " + meshNames.length);
	return undefined;
    }

    console.log("found mesh: " + meshNames[0]);
    let meshName = meshNames[0];

    return objs[meshName];
}

function findSubset(subsetName, subsets) {
    return subsets.filter(o => o.name === subsetName)[0];
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
    // There's probably a more generic way to do this based on the Element type and geometry-processing.js types of the function
    if (fname === "StarV") {
	let argObj = findMesh(fnArgs[0], json, objs);
	res = doStar(fnCall, mappings, argObj);
    } else if (fname === "Closure") {
	let argObj = objs[fnArgs[0]];
	res = doClosure(fnCall, mappings, argObj);
    } else if (fname === "Link") {
	let argObj = objs[fnArgs[0]];
	res = doLink(fnCall, argObj);
    } else if (fname === "LinkV") {
	let argObj = findMesh(fnArgs[0], json, objs);
	res = doLinkV(fnCall, mappings, argObj);
    } else if (fname === "Boundary") { // Supposed to only operate on a SComplex; here we look for a mesh subset
	let argObj = objs[fnArgs[0]];
	res = doBoundary(fnCall, mappings, argObj);
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
	let positions = sc.geometry.positions;

	for (let v of mesh.vertices) {
	    let vi = v.index;
	    let vname = substitute(plugin2sub, objName(cname, vtype, vi));
	    let vpos = positions[vi]; // Vector object, throw away z pos

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
    let subPreds = json.constraints.predicates;
    let subFnCalls = json.constraints.functions;

    let objs_flat = subDecls.map(decl => makeObj(decl))
	.filter(o => o !== undefined);

    let objs = [];
    for (let obj of objs_flat) {
	objs[obj.name] = obj; // Convert to a key-value store
    }

    // Deal with statements that select a subpart of a whole (TODO: currently just a vertex of an SC)
    let mappings = { sub2plugin: {}, plugin2sub: {} };
    let finalObjs = objs;

    let objs_flat_final = [];
    for (var objName in finalObjs) { // Convert from key-value store back to flat list of objects
	objs_flat_final.push(finalObjs[objName]);
    }

    // Output Substance code
    console.log("final objs:", finalObjs, objs_flat_final);
    let lines = _.flatten(objs_flat_final.map(o => makeProg(mappings, o)));
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

    console.log("writing Substance program: ", newSub);
    Fs.writeFileSync('Sub_instantiated.sub', newSub);

    console.log("writing values for Style: ", styVals);
    Fs.writeFileSync('values.json', styVals);

    console.log("ending mesh plugin")
}

main();

module.exports = {main, makeSubAndValues, global_mesh};
