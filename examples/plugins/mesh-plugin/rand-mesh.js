// The function `makeRandMesh` makes a random Delaunay mesh with `numPts`  points 
// whose coordinates are each sampled uniformly at random from range `range`
// and outputs it as a .obj file.
// For how the Delaunay triangulation works, see: https://github.com/mapbox/delaunator

const Fs = require('fs');
const _ = require('lodash');
var seedrandom = require('seedrandom');
const Delaunator = require('delaunator');

var seed1; // MUTABLE STATE, set up in setUpRand

// uniform sampling
function sampleFrom(range) {
    let [left, right] = range;
    if (right <= left) { throw "invalid range"; }
    let len = right - left;

    return Math.random() * len + left;
}

// Parameters:
// Range of a coordinate [a, b]
// Number of points n
// Returns a list of point
function samplePts(numPts, range) {
    let pts = [];

    for (let i = 0; i < numPts; i++) {
	let pt = [ sampleFrom(range),
		   sampleFrom(range) ];
	pts.push(pt);
    }

    return pts;
}

function makeRandMesh(numPts, range) {
    const points = samplePts(numPts, range);
    // console.log("points", points);

    const delaunay = Delaunator.from(points);
    const triangles = delaunay.triangles
    // console.log("triangles", triangles);

    // let coordinates = [];

    // // Not needed, just for debugging
    // for (let i = 0; i < triangles.length; i += 3) {
    // 	let tri = [
    // 	    points[triangles[i]],
    // 	    points[triangles[i + 1]],
    // 	    points[triangles[i + 2]]
    // 	];
    // 	coordinates.push(tri);
    // }

    // console.log("coordinates", coordinates);

    // Output obj file
    let obj = [];

    // Output the vertices
    // Assuming mesh is planar, so z = 0
    for (let pt of points) {
	let line = ["v", pt[0].toString(), pt[1].toString(), "0"].join(" ");
	obj.push(line);
    }

    // Output the faces
    // In obj files, mesh vertices are indexed from 1, not 0, so add 1 to each index
    let triangles1 = triangles.map(i => i + 1);
    
    for (let i = 0; i < triangles.length; i += 3) {
	let line = ["f", triangles1[i], triangles1[i + 1], triangles1[i + 2]].join(" ");
	obj.push(line);
    }
    
    let obj_final = obj.join("\n");
    console.log("final .obj", obj_final);
    return obj_final;
}

function setUpRand(seed) {
    seed1 = seed;
    // Global PRNG: set Math.random.
    seedrandom(seed1, { global: true });
    console.log("Math.rand in rand-mesh", Math.random());
}

function main() { // An example use
    const numPts = 4;
    const range = [-5, 5];
    let res = makeRandMesh(numPts, range);
}

module.exports = { setUpRand, makeRandMesh, sampleFrom };
