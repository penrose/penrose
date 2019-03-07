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

function mkE(ename, cname, vname1, vname2) {
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

    // Construct a simplicial complex for a mesh 
    let SCO = new SC.SimplicialComplexOperators(mesh);

    // Build mappings from Penrose name to mesh index (not really needed since each name includes its index...)
    let penrose_to_mesh = {};

    // global_mesh = mesh; // TODO remove

    // Map names to indices
    for (let v of mesh.vertices) {
	// TODO document these conventions
	penrose_to_mesh[objName(cname, vtype, v.index)] = v.index;
    }

    for (let e of mesh.edges) {
	penrose_to_mesh[objName(cname, etype, e.index)] = e.index;
    }

    for (let f of mesh.faces) {
	penrose_to_mesh[objName(cname, ftype, f.index)] = f.index;
    }

    return { type: 'SimplicialComplex',
	     name: cname,
	     mesh: mesh, 
	     sc: SCO, 
	     mappings: penrose_to_mesh
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
    // console.log("sc: ", SCO);
    // console.log("sc star: ", star_sc);

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
    let inputSubsetName = stmt.fargNames[0];
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
	    prog.push(label(cname, ename, etype, e.index));
	}
    }

    for (let f of mesh.faces) {
	let fname = objName(cname, ftype, f.index);

	if (!wasRemapped(plugin2sub, fname)) {
	    prog.push(decl(ftype, fname));
	    prog.push(label(cname, fname, ftype, f.index));
	}
    }

    // Build connectivity info for edges and faces
    // Substitute any renamed objects, if they exist
    for (let v of mesh.vertices) {
	let vname = substitute(plugin2sub, objName(cname, vtype, v.index));

	for (let e of v.adjacentEdges()) {
	    let ename = objName(cname, etype, e.index);
	    prog.push(inVE(vname, ename)); // TODO use mkE
	}
    }

    // TODO: check what faces should be in the star
    for (let f of mesh.faces) {
	let edges = [];

	for (let e of f.adjacentEdges()) {
	    edges.push(e.index);
	}

	let enames = edges.map(i => substitute(plugin2sub, objName(cname, etype, i)));

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
function findMesh(vname, json, scObjs) {
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

    // TODO: use a better find function
    // expects exactly one mesh with the name
    let mesh = scObjs.filter(o => o.name === meshName)[0];

    return mesh;
}

function findSubset(subsetName, subsets) {
    return subsets.filter(o => o.name === subsetName)[0];
}

function makeNameMappings(json, scObjects) {
    // Deal with named vertex statements.
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
    let vertexMappings = json.objects
	.filter(o => o.objType === 'Vertex');

    for (let v of vertexMappings) {
	let vname = v.objName;
	let scObj = findMesh(vname, json, scObjects);
	let mesh = scObj.mesh;
	let scName = scObj.name;

	subToAssignedMapping[vname] = { scName: scName, // Should this be a pointer to the SC?
					assignedName: undefined,
					index: undefined };

	if (mesh.vertices.length == 0) {
	    console.log("error: mesh has no vertices!");
	} else {
	    let vi = Math.floor(Math.random() * mesh.vertices.length);
	    // let vi = 4; // TODO: hardcode to the center vertex
	    let assignedName = objName(scName, vtype, vi); // K1_V0

	    subToAssignedMapping[vname].assignedName = assignedName;
	    subToAssignedMapping[vname].index = vi;

	    assignedToSubMapping[assignedName] = vname;
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

// Examine an object and call the right Sub prog generating function
function makeProg(mappings, o) {
    if (o.type === 'SimplicialComplex') {
	return scToSub(mappings, o);
    } else if (o.type === 'MeshSubset') {
	return subsetToSub(mappings, o);
    }
}

function makeSub(json) {
    /* How this plugin handles Substance programs:
       - Make a new simplicial complex for every SComplex declaration
       - Names declared vertices, edges, faces according to what they are in (TODO work this out)
       - Performs star, closure, and link functions, naming them correctly (TODO work this out)
       - Ignores all other Substance statements (for now) */

    // We only construct simplicial complexes. Objects are named according to the SC they belong to
    let scObjects = json.objects
	.filter(o => o.objType === 'SComplex')
	.map(o => makeSComplex(o.objName));
    console.log("complexes", scObjects);

    // Deal with statements that select a subpart of a whole (currently just a vertex of an SC)
    let mappings = makeNameMappings(json, scObjects);

    // TODO: deal w/ star being called arbitrary # times
    // Perhaps this should be done recursively?
    let starObjects = json.constraints.functions
	.filter(o => o.fname === 'StarV')
        .map(o => doStar(o, mappings, findMesh(o.fargNames[0], json, scObjects)));
    console.log("stars", starObjects);

    let clObjects = json.constraints.functions
	.filter(o => o.fname === 'Closure')
        .map(o => doClosure(o, mappings, findSubset(o.fargNames[0], starObjects)));
    // TODO: factor out findMesh / findSubset
    console.log("cls", clObjects);

    // Generate Substance programs for all objs
    // TODO: use consistent fp style
    let objs = _.concat(scObjects, starObjects, clObjects);
    let lines = _.flatten(objs.map(o => makeProg(mappings, o)));
    let subProgStr = lines.join(newline);

    return subProgStr;
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
