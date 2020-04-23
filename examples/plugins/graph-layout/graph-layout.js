// "Hello world" code from https://github.com/dagrejs/dagre/wiki#using-dagre

const Fs = require('fs');
const _ = require('lodash');
const dagre = require("dagre");

const crash = () => { console.log("crashing"); console.log(null[0]); }

// TODO: Estimate these constants better
// TODO: This should actually be the size of the ellipse that contains the node, not the text size
const letterWidth = 100; // Pixels
const letterHeight = 80;
const ellipseLR_Padding = 600;
// const ellipseTB_Padding = 600;
const ellipseTB_Padding = 1800;

function buildGraph(json) {
    let subDecls = json.objects;
    let subPreds = json.constraints.predicates;
    // let subFnCalls = json.constraints.functions;

    // Create a new directed graph
    var g = new dagre.graphlib.Graph();

    // Set an object for the graph label
    g.setGraph({});

    // Default to assigning a new object as a label for each new edge.
    g.setDefaultEdgeLabel(function() { return {}; });

    // Add nodes to the graph. The first argument is the node id. The second is metadata about the node (labels)
    for (let obj of subDecls) {
	// console.log("obj", obj);
	if (obj.objType !== 'Node') {
	    console.error("Note: object type is not Node, but", obj.objType);
	    // TODO: If there are any non-node subtypes, check for them
	    // crash();
	}

	g.setNode(obj.objName, { label: "", width: obj.objName.length * letterWidth + ellipseLR_Padding, height : letterHeight + ellipseTB_Padding });
    }

    // Add edges to the graph.
    for (let pred of subPreds) {
	// console.log("pred", pred);
	if (pred.pname !== 'to') {
	    console.error("Note: predicate type is not `to`, but", pred.pname);
	    crash();
	}

	let predArgs = pred.pargs.map(arg => arg.Left);
	g.setEdge(predArgs[0], predArgs[1]);
    }

    // Lay out graph
    // https://github.com/dagrejs/dagre/wiki#configuring-the-layout
    // TODO: Explore layout configuration options
    // TODO: This does not seem to work
    let layout_config = { nodesep: 100, edgesep: 100, ranksep: 500 };
    g.nodesep = 1000;
    dagre.layout(g, layout_config);

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
    return { graph: g, layout_info: layout_info };
}

// TODO: This does not yet read the Substance graph yet, so it's just rendering part of the premade sample graph above if the names in the given Substance program are the same
function makeSty(layout_result) {
    let layout_info = layout_result.layout_info;
    let graph = layout_result.graph;
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

    let graph_json = {
	subName : "dagre_graph",
	nameVals : [ { propertyName: "width",
		       propertyVal: graph.graph().width },
		     { propertyName: "height",
		       propertyVal: graph.graph().height } 
		   ]
    };

    vals.push(graph_json);

    return JSON.stringify(vals);
}

function main() {
    console.log("starting graph-layout plugin");

    let rawdata = Fs.readFileSync('Sub_enduser.json');
    let subJSON = JSON.parse(rawdata);
    console.log("Received JSON", JSON.stringify(subJSON));

    let seeds = subJSON.params.map(x => x.contents.contents);
    console.log("seeds", seeds);

    // TODO: We don't have node dimensions from the frontend, so we just estimate them in the plugin
    let layout_result = buildGraph(subJSON.substance);
    let newSub = ""; // No new Substance lines
    let styVals = makeSty(layout_result); // Convert laid-out graph (node coords) to JSON format for Style

    console.log("writing Substance program: ", newSub);
    Fs.writeFileSync('Sub_instantiated.sub', newSub);

    console.log("writing values for Style: ", styVals);
    Fs.writeFileSync('values.json', styVals);

    console.log("ending graph-layout plugin")
}

main();

module.exports = {main};
