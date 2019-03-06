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
    // TODO: make a simple random mesh, not a constant one
    // Tetrahedron has vertices indexed from 0-3 (inclusive), edges 0-5, faces 0-3
    // Maybe I should draw a 2D mesh
    let polygonSoup = MeshIO.readOBJ(TetraMesh);
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
function doStar(starName, scObj) {
    // Select the vertices, edges, and faces that are in the star
    // TODO do programmatically
    let cname = scObj.name;
    let SCO = scObj.sc;

    let selectedSimplices = new MeshSubset();
    selectedSimplices.addVertices([1]);
    selectedSimplices.addEdges([]);
    selectedSimplices.addFaces([]);

    let star_sc = SCO.star(selectedSimplices);
    console.log("sc: ", SCO);
    console.log("sc star: ", star_sc);

    return { type: 'Star',
	     name: starName, // name bound in Substance
	     scName: cname,
	     selectedSimplices: selectedSimplices,
	     starObj: star_sc,
	   };
}

// Return the Substance programs
function scToSub(scObj) {
    let mesh = scObj.mesh;
    let cname = scObj.name;
    let prog = [];

    // Emit all declarations first, since objects need to be defined in Substance before they're referenced
    // TODO factor out V,E,F code and 3-tuple
    for (let v of mesh.vertices) {
	prog.push(decl(cname, vtype, v.index));
	prog.push(label(cname, vtype, v.index));
    }

    for (let e of mesh.edges) {
	prog.push(decl(cname, etype, e.index));
	prog.push(label(cname, etype, e.index));
    }

    for (let f of mesh.faces) {
	prog.push(decl(cname, ftype, f.index));
	prog.push(label(cname, ftype, f.index));
    }

    // Build connectivity info for edges and faces
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

    return prog;
}

function starToSub(starWrapper) {
    let cname = starWrapper.scName;
    let star_sc = starWrapper.starObj;

    let prog = [];

    // Note that for a simplex, an object is simply the index
    for (let v of star_sc.vertices) {
	console.log("selected vertex: ", v);
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

// Examine an object and call the right Sub prog generating function
function makeProg(o) {
    if (o.type === 'SimplicialComplex') {
	return scToSub(o);
    } else if (o.type === 'Star') {
	return starToSub(o);
    }
}

function makeSub(json) {
    /* How this plugin handles Substance programs:
       - Make a new simplicial complex for every SComplex declaration
       - Names declared vertices, edges, faces according to what they are in (TODO work this out)
       - Performs star, closure, and link functions, naming them correctly (TODO work this out)
       - Ignores all other Substance statements (for now) */

    // We only construct simplicial complexes, and objects are named according to the SComplex they belong to
    let scObjects = json.objects
	.filter(o => o.objType === 'SComplex')
	.map(o => makeSComplex(o.objName));

    console.log("complexes", scObjects);

    // Mappings from Substance name to subpart of structure
    // Have to figure out which complex it's in
    let nameMappings = {};
    let vertexMappings = json.objects
	.filter(o => o.objType === 'Vertex');

    for (let v in vertexMappings) {
	// nameMappings.v
    }

    // TODO: deal with named vertex statements.
    // Choose a random vertex in the mesh (fail if no vertices)
    // Make a mapping from Substance name ("v") to that vertex's index, simplicial complex, and generated name ("K1_v1")
    // What info do we need about "v"? Do we ever need a reverse mapping?

    // When a Sub prog is generated:
    // Don't output the Vertex or Label statement, but do output the In edge statement
    
    // This only deals with Vertex v in S (where SimplicialComplex S), not other kinds of objects in non-sc objects

    // TODO: deal w/ star being called arbitrary # times
    // Perhaps this should be done recursively?

    // TODO: output the In statements for star, link, and closure according to what they're bound to
    let starObjects = json.constraints.functions
	.filter(o => o.fname === 'StarV')
        .map(o => doStar(o.varName, findMesh(o.fargNames[0], json, scObjects)));

    console.log("stars", starObjects);

    // Generate Substance programs for all objs
    // TODO: use consistent fp style
    let objs = _.concat(scObjects, starObjects);
    let lines = _.flatten(objs.map(o => makeProg(o)));
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
