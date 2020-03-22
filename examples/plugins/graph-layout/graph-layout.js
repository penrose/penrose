// "Hello world" code from https://github.com/dagrejs/dagre/wiki#using-dagre

const Fs = require('fs');
const _ = require('lodash');
const dagre = require("dagre");

function makeSampleGraph() {
    // Create a new directed graph
    var g = new dagre.graphlib.Graph();

    // Set an object for the graph label
    g.setGraph({});

    // Default to assigning a new object as a label for each new edge.
    g.setDefaultEdgeLabel(function() { return {}; });

    // Add nodes to the graph. The first argument is the node id. The second is
    // metadata about the node. In this case we're going to add labels to each of
    // our nodes.
    g.setNode("kspacey",    { label: "Kevin Spacey",  width: 144, height: 100 });
    g.setNode("swilliams",  { label: "Saul Williams", width: 160, height: 100 });
    g.setNode("bpitt",      { label: "Brad Pitt",     width: 108, height: 100 });
    g.setNode("hford",      { label: "Harrison Ford", width: 168, height: 100 });
    g.setNode("lwilson",    { label: "Luke Wilson",   width: 144, height: 100 });
    g.setNode("kbacon",     { label: "Kevin Bacon",   width: 121, height: 100 });

    // Add edges to the graph.
    g.setEdge("kspacey",   "swilliams");
    g.setEdge("swilliams", "kbacon");
    g.setEdge("bpitt",     "kbacon");
    g.setEdge("hford",     "lwilson");
    g.setEdge("lwilson",   "kbacon");

    // Lay out graph
    // TODO: Explore layout configuration options
    dagre.layout(g);

    let layout_info = {};

    // Print layout information
    g.nodes().forEach(function(v) {
	console.log("Node " + v + ": " + JSON.stringify(g.node(v)));
	
	const node = g.node(v);
	layout_info[v] = { x: node.x, y: node.y };
    });

    // TODO put in edge info and render edges
    g.edges().forEach(function(e) {
	console.log("Edge " + e.v + " -> " + e.w + ": " + JSON.stringify(g.edge(e)));
    });

    console.log("layout info", layout_info);
    return layout_info;
}

// TODO: This does not yet read the Substance graph yet, so it's just rendering part of the premade sample graph above if the names in the given Substance program are the same
function makeSty(layout_info) {
    let vals = [];

    for (let nodeName of Object.keys(layout_info)) {
	let info = layout_info[nodeName];
	let local_pos_x = { propertyName: "x",
			    propertyVal: info.x };
	let local_pos_y = { propertyName: "y",
			    propertyVal: info.y };
	let local_vals = [local_pos_x, local_pos_y];

	let local_json = {};
	local_json["subName"] = nodeName;
	local_json["nameVals"] = local_vals;

	vals.push(local_json);
    }

    return JSON.stringify(vals);
}

function makeSubAndValues(json, layout_info) {
    // let res = makeSub(json);

    let subProgStr = "";
    let styVals = makeSty(layout_info);

    return { newSub: subProgStr,
	     styVals: styVals
	   };
}

function main() {
    console.log("starting graph-layout plugin");

    let rawdata = Fs.readFileSync('Sub_enduser.json');
    let subJSON = JSON.parse(rawdata);
    console.log("Received JSON", JSON.stringify(subJSON));

    let seeds = subJSON.params.map(x => x.contents.contents);
    console.log("seeds", seeds);

    // TODO: Build graph from Substance program in makeSampleGraph
    // TODO: We don't have node dimensions from the frontend, so just estimate them in the plugin
    let layout_info = makeSampleGraph();

    let results = makeSubAndValues(subJSON.substance, layout_info);
    let [newSub, styVals] = [results.newSub, results.styVals];

    console.log("writing Substance program: ", newSub);
    Fs.writeFileSync('Sub_instantiated.sub', newSub);

    console.log("writing values for Style: ", styVals);
    Fs.writeFileSync('values.json', styVals);

    console.log("ending graph-layout plugin")
}

main();

module.exports = {main};
