// Ported from ./MeshOptimize.cpp (see comments in that file for notes)

const Fs = require('fs');
const _ = require('lodash');

const Vector = require('./geometry-processing-js/node/linear-algebra/vector.js');
const Vertex = require('./geometry-processing-js/node/core/vertex.js');
const Edge = require('./geometry-processing-js/node/core/edge.js');
const Halfedge = require('./geometry-processing-js/node/core/halfedge.js');
const MeshSubset = require('./geometry-processing-js/node/core/mesh-subset.js');
const Mesh = require('./geometry-processing-js/node/core/mesh.js');
const Geometry = require('./geometry-processing-js/node/core/geometry.js');
const MeshIO = require('./geometry-processing-js/node/utils/meshio.js');

const RandMesh = require('./rand-mesh.js');

const square = (x) => x * x;

function energy(mesh, positions, avgLen) {
    console.log("energy avgLen", avgLen);

    // initialize energy to 0
    let e_sum = 0.0;

    // add contribution of edge length energy
    for (let e of mesh.edges) {
	let v1 = e.halfedge.vertex;
	let v2 = e.halfedge.twin.vertex;
	// console.log("edge", e.index, "vertex", v1.index, "pos", positions[v1], positions[v2]);
	let u = positions[v1].minus(positions[v2]); // vector
	e_sum += square( u.norm2() - avgLen ) / 4.0; // square is square, not square root
	// console.log("e_sum", e_sum);
    }

    // add contribution of area energy
    const A0 = avgLen * Math.sqrt(3.0) / 2.0; // sqrt is square root, not square

    for (let f of mesh.faces) {
	let va = f.halfedge.vertex;
	let vb = f.halfedge.next.vertex;
	let vc = f.halfedge.next.next.vertex;
	// console.log("va, vb, vc", va.index, vb.index, vc.index);
	let a = positions[va]; // vectors
	let b = positions[vb];
	let c = positions[vc];
	// console.log("a, b, c", a, b, c);

	let A = b.minus(a).cross(c.minus(a)).z; // (b-a) X (c-a)
	e_sum += square(A0 - A) / 2.0;
	// console.log("A, e_sum", A, e_sum);
    }

    return e_sum;
}

const pointRange = [-1, 1]; // Range to sample mesh points from (geometry)
const numPointsRange = [5, 9];

function optimizeMesh() {
    console.log("optimize mesh");

    // --------------- Set up mesh
    const numPts = 7; // Math.floor(RandMesh.sampleFrom(numPointsRange));
    console.log("num pts", numPts);
    let mesh_input = RandMesh.makeRandMesh(numPts, pointRange);
    console.log("mesh input", mesh_input);
    let polygonSoup = MeshIO.readOBJ(mesh_input);
    console.log("polygon soup", polygonSoup);
    let mesh = new Mesh.Mesh();
    mesh.build(polygonSoup);
    console.log("mesh", mesh);

    // Note: geometry is rebuilt after SCO is built, so mesh indices are accurate
    let geometry = new Geometry.Geometry(mesh, polygonSoup["v"], false);
    let positions = geometry.positions;
    console.log("positions", positions);
    console.log("positions length", Object.keys(positions).length);
    console.log("vertices length", mesh.vertices.length);

    // --------------- Optimize mesh
    // parameters for energy
    const L = geometry.meanEdgeLength(); // target edge length---note that we shouldn't compute this within energh() since the mean edge length will change throughout optimization
    const A0 = L * Math.sqrt(3.0) / 2.0; // (twice) the area of corresponding equilateral triangle

    // parameters for gradient descent
    const maxGradientSteps = 10000;
    const epsilon = 1e-6; // stopping tolerance

    let gradients = {}; // indexed by vertex index (which gets converted to string), mapped to vector
    let step;

    // take gradient descent steps
    for (step = 0; step < maxGradientSteps; step++) {
        // initialize gradient to zero
	for (let v of mesh.vertices) {
	    gradients[v] = new Vector(0.0, 0.0, 0.0);
	}

	console.log("gradients", gradients);

	// add gradient of edge length energy
	for (let e of mesh.edges) {
            // get endpoints of edge e = (v1,v2)
	    let v1 = e.halfedge.vertex;
	    let v2 = e.halfedge.twin.vertex;

	    // compute edge vector
	    let u = positions[v1].minus(positions[v2]); // vector

	    // add gradient contribution at each endpoint
	    let u_scaled = u.times(u.norm2() - L);
	    gradients[v1] = gradients[v1].plus(u_scaled);
	    gradients[v2] = gradients[v2].minus(u_scaled);
	}

	console.log("gradients", gradients);

        // add gradient of (signed) triangle area energy
	for (let f of mesh.faces) {
	    // get vertices of triangle f = (va, vb, vc)
	    let va = f.halfedge.vertex;
	    let vb = f.halfedge.next.vertex;
	    let vc = f.halfedge.next.next.vertex;

            // get positions of the three vertices (vectors)
	    let a = positions[va];
	    let b = positions[vb];
	    let c = positions[vc];

            // compute (twice) the triangle area
	    let A = b.minus(a).cross(c.minus(a)).z; // (b-a) X (c-a)

	    console.log("A", A);
	    console.log("va grad x", gradients[va].x);

            // add gradient contribution at vertex a
            gradients[va].x += (A0-A)*(c.y-b.y);
            gradients[va].y += (A0-A)*(b.x-c.x);

            // add gradient contribution at vertex b
            gradients[vb].x += (A0-A)*(a.y-c.y);
            gradients[vb].y += (A0-A)*(c.x-a.x);

            // add gradient contribution at vertex c
            gradients[vc].x += (A0-A)*(b.y-a.y);
            gradients[vc].y += (A0-A)*(a.x-b.x);
	}

	console.log("gradients", gradients);

        // Part 2: Take a gradient step =========================
	let E0 = energy(mesh, positions, L); // get the energy of the current configuration
	console.log("E0", E0);

        // compute the squared norm of the gradient
        let gradNormSquared = 0.0;
	let originalPositions = {}; // indexed by vertex index; maps to vector
	// TODO: can we actually just index by vertex?

	for (let v of mesh.vertices) {
	    originalPositions[v] = positions[v];
	    gradNormSquared += gradients[v].norm2();
	}

	console.log("originalPositions", originalPositions);
	console.log("gradNormSquared", gradNormSquared);

        // perform backtracking line search, as described in
        // Body & Vandenberghe, "Convex Optimization" (Algorithm 9.2)
        let tau = 1.0; // time step
        const alpha = 0.25; // sufficient decrease parameter
        const beta = 0.5; // backtracking parameter
        const nMaxBacktrackingSteps = 100;

        for (let i = 0; i < nMaxBacktrackingSteps; i++) {
	    console.log("tau", tau);

            // move to tentative new configuration
	    for (let v of mesh.vertices) {
		// TODO / NOTE: this actually changes the positions array. Should it be immutable?
		positions[v] = originalPositions[v].minus(gradients[v].times(tau));
	    }

            // evaluate energy at new configuration
            const E = energy(mesh, positions, L);
	    console.log("new E", E);

            // if there was a sufficient decrease in energy, stop
            if (E < (E0 - alpha * tau * gradNormSquared)) { console.log("sufficient decrease in energy"); break; }

            // otherwise, shrink the time step
	    console.log("shrinking timestep");
            tau *= beta;
	}

        // if the gradient is small enough, we're at a local minimum and can stop
        if (Math.sqrt(gradNormSquared) < epsilon) { console.log("grad norm squared is small enough", gradNormSquared); break; }
    }

    if (step == maxGradientSteps) {
        console.error("Warning: did not reach requested tolerance ", epsilon, " after ", step, " steps.");
    } else {
        console.error("Converged to a tolerance of ", epsilon, " after ", step, " steps.");
    }

    return positions;
}

function main() {
    let res = optimizeMesh();
    console.log("optimize mesh results", res);
}

main();

module.exports = { energy, optimizeMesh };
