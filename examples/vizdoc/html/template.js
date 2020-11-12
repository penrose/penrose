
import * as d3 from "d3"
import { line } from "d3";

   // NB - when appending either below lines work. second one doesn't involve function call
// d3.select("#inputs").append(function() {return x})
//document.getElementById("inputs").appendChild(x);

// todo - style labels in css
// todo - type conversions
// TODO - canvas conversions
// todo - replace lets with consts? :/

// Q - make customizable? seems unnecessary
const [svgwidth, svgheight] = [600, 300];
let transformables;    // todo - move into function??
const attrdict = {
    "color": {
        "fullargs": 4,
        "default": "#000000"
    },
    "range": {
        "fullargs": 6
    },
    "dd": {
        "fullargs": 5,
        "default": ""
    },
    "text": {
        "fullargs": 4,
        "default": "{}"
    },
    "number": {
        "fullargs": 6,
        "defaultmin": "",
        "defaultmax": "",
        "default": ""
    }
}

// ename - what user wants to call their attribute
// eattr - what svg calls that attribute

// Make the input element for a color attribute
function makeIColor(ename, eattr, evalue, subpath) {
    let newin = document.createElement("input");
    newin.type = "color";
    newin.id = ename;
    newin.value = evalue;
    newin.oninput = (!subpath) ? function() {update(eattr, this.id, this.value);} : function() {updateMulti(eattr, this.id, this.value, subpath);};
    return newin;
}

// Make the input element for a ranged attribute
// evalue not default bc range might exclude 0
function makeIRange(ename, eattr, min, max, evalue, subpath) {
    let newin = document.createElement("input"); // Q - move elem creation to caller?
    newin.type = "range";
    newin.id = ename;
    newin.min = min;
    newin.max = max;
    newin.value = evalue;
    newin.oninput = (!subpath) ? function() {update(eattr, this.id, this.value, true);} : function() {updateMulti(eattr, this.id, this.value, subpath, true);};
    return newin;
}

// Make text input element
function makeIText(ename, eattr, evalue, subpath) {
    let newin = document.createElement("input");
    newin.type = "text";
    newin.id = ename;
    newin.value = evalue;
    newin.oninput = (!subpath) ? function() {update(eattr, this.id, this.value);} : function() {updateMulti(eattr, this.id, this.value, subpath);};
    return newin;
}

// Make number input element
function makeINumber(ename, eattr, min, max, evalue, subpath) {
    let newin = document.createElement("input");
    newin.type = "number";
    newin.id = ename;
    newin.min = min;
    newin.max = max;
    newin.value = evalue;
    newin.oninput = (!subpath) ? function() {update(eattr, this.id, this.value);} : function() {updateMulti(eattr, this.id, this.value, subpath);};
    return newin;
}

// TODO - make sure ddict is "evaled" before passing to here
// ddict in form {"choice: svgvalue"} e.g. {"solid": "1, 0", "dashed": "7, 5"} for stroke-dasharray
// Make dropdown element
function makeIDropdown(ename, eattr, ddict, sel, subpath) {
    let newin = document.createElement("select");
    newin.id = ename;
    newin.onchange = (!subpath) ? function() {update(eattr, this.id, this.value);} : function() {updateMulti(eattr, this.id, this.value, subpath);};
    for (let key in ddict) {
        let option = document.createElement("option");
        option.value = ddict[key];  // could always pass ddict to update and then store ename in option value
        option.text = key;
        if (key === sel) option.selected = true;
        newin.appendChild(option);
    };
    return newin;
}

// span param- do we need to update a corresponding span text?
// NB - if span is made customizable we need to change how this is called
function update(eattr, id, val, span = false) {
    const shape = d3.select("#theshape");
    modShape(shape, eattr, val);
    if (span) d3.select("#" + id + "-val").text(val);
    return;
}

function updateMulti(eattr, id, val, subpath, span = false) {
    const shape = d3.select("#theshape");
    let spath = subpath.split(" ");    // split to find substitutions
    for (let wd in spath) {
        if (wd[0] == "$" && wd[-1] == "$") {    // if word needs to be substituted
            let swd = wd.slice(1, -1);
            if (swd == id) {
                wd = val;
            }
            else {      // retrieve value from input
                wd = document.getElementById(swd).value;    // Q - is this the best way to do it? multiple ways...
            }
        }
    }
    modShape(shape, eattr, spath.join(" "));
    if (span) d3.select("#" + id + "-val").text(val);
    return;
}

// Q - users could be able to choose if they want span in annotations rather than based on input types
// on the other hand that just makes more annotation work - could make optional...
function makeLabel(ename, evalue, span = false) {
    let newlab = document.createElement("label");
    newlab.htmlFor = ename;
    if (span) {newlab.innerHTML = ename + " = <span id=" + ename + "-val>" + evalue + "</span"} // todo - should probably be tested
    else {newlab.textContent = ename};
    return newlab;
}

// ensure passing subpath separately
function addInputandLabel(para, wds, subpath = "") {
    let typ = wds[2];   // type of input (including selection)
    let ename = wds[0];
    let eattr = wds[1];
    let evalue = wds[wds.length - 1];   //always last element
    switch (typ) {      // TODO - expand with more inputs
        case "color":
            para.append(() => makeIColor(ename, eattr, evalue, subpath));
            para.append(() => makeLabel(ename, evalue));
            break;
        case "range":
            para.append(() => makeIRange(ename, eattr, wds[3], wds[4], evalue, subpath));
            para.append(() => makeLabel(ename, evalue, true));
            break;
        case "text":
            para.append(() => makeIText(ename, eattr, evalue, subpath));
            para.append(() => makeLabel(ename, evalue));
            break;
        case "dd":
            let ddict = JSON.parse(wds[3]);    
            para.append(() => makeIDropdown(ename, eattr, ddict, evalue, subpath));
            para.append(() => makeLabel(ename, evalue));
            break;
        case "number":
            para.append(() => makeINumber(ename, eattr, wds[3], wds[4], evalue, subpath));
            para.append(() => makeLabel(ename, evalue));
            break;
        default:
            throw "Input type not recognized.";         // todo: enumerate valid input types here
    }
    return;

}

function transformVal(eattr, evalue) {
    // Q - better way of doing this? will the if statement always work correctly?
    if (transformables[eattr] == "x") return (+evalue + svgwidth / 2).toString()
    else if (transformables[eattr] == "y") return (-(+evalue) + svgheight / 2).toString()
    else throw "Invalid entry in transform dictionary. Values must be `x` or `y`.";
}

function modShape(shape, eattr, evalue) {
    let eattrs = (eattr[0] == "[") ? parseArr(eattr) : [eattr];
    eattrs.forEach(attrib => {
        if (transformables.hasOwnProperty(attrib)) {
            shape.attr(attrib, transformVal(attrib, evalue))}
        else shape.attr(attrib, evalue);
    });
    // TODO - toss error if attr isn't applicable or DNE
    return;
}

// NB last element of full line is always the value
// if user didn't add an optional value for an attr, we add it in here
// todo - all argument validation 
// todo - move text reformatting somewhere else? not exactly encapsulating here...
// if single is false, we add one to argument length to accomodate substitution path - multi
function addDefaults(wds) {
    let typ = wds[2]  // type of input (including selection)  // todo - change order?
    let fullargs = attrdict[typ]["fullargs"];
    let def = (attrdict[typ]["default"]) ? attrdict[typ]["default"] : "";   // if it has default
    switch (typ) {
        case "color":
            if (wds.length == fullargs - 1) wds.push(def)
            else if (wds.length != fullargs) throw "Wrong number of arguments for color input!";
            break;
        case "dd":
            if (wds.length == fullargs - 1) wds.push(def)
            else if (wds.length != fullargs) throw "Wrong number of arguments for dropdown input!";
            break;
        case "text":
            if (wds.length == fullargs - 1) wds.push(def)
            else if (wds.length != fullargs) throw "Wrong number of arguments for text input!";
            wds[wds.length - 1] = wds[wds.length - 1].slice(1, -1); // remove braces - reformat
            break;
        case "number":
            if (wds.length == fullargs - 3) wds.push(attrdict[typ]["defaultmin"], attrdict[typ]["defaultmax"], def) // push min, max, default
            else if (wds.length == fullargs - 4) {     // push min, max
                let val = wds[3];
                wds = wds.slice(0, 3);
                wds.push(attrdict[typ]["defaultmin"], attrdict[typ]["defaultmax"], val);
            }
            else if (wds.length != fullargs) throw "Wrong number of arguments for number input!";
            break;
        default:
            break;
            // cases with all non-optional attributes (like range) will hit here 
            // todo - argument # validation
    }
    return wds;
}

// could use eval but...
function parseArr(strarr) {
    return strarr.slice(1, -1).replace(" ", "").split(",")  // remove brackets, strip whitespace, and split
}

// many user attribs to one svg attrib
// this so far only works for string substitution.. eg. {$startx$ $starty$}
// how to implement possibilities like adding? without veering into peril w/ eval
function parseAndEvalMTO(wds) {
    const lines = []
    const uwds = parseArr(wds[0]);
    const typ = wds[1];
    const subpath = wds[2];
    let spath = subpath.split(" ");    // split to find substitutions
    const adict = JSON.parse(wds[3]);   // for dict - go back and do sugar with one default spread out
    for (const wd in uwds) {
        let line = [wd].concat(typ, adict[wd]);
        line = addDefaults(line);
        const newp = d3.select("#inputs").append("p");
        addInputandLabel(newp, line, subpath);
        lines.push(line);
        for (let wd in spath) {
            if (wd[0] == "$" && wd[-1] == "$" && wd.slice(1, -1) == uwd) {    // if word needs to be substituted

            }
    }
    }
    
}


// x cx range -300 300 0
function parseLn(line, shape) {
    // todo - test regex with more special characters
    // todo - this currently messes up with nested parentheses/brackets
    let wds = line.match(/(?:[^\s{}[\]]+|{[^{}]*}|\[[^[\]]*\])+/g); // split on spaces except when in curly or square brackets
    const newp = d3.select("#inputs").append("p");

    // if (line[0][0] == "[") {
    //     parseAndEvalMTO(wds)
    // }
    wds = addDefaults(wds);
    addInputandLabel(newp, wds);
    modShape(shape, wds[1], wds[wds.length-1]);
    return;
}

// todo - x canvas default width side
// todo - validate ranges for coords?
// todo  - transform rectangle - originally top right corner

function main(lines) {
    const linearr = lines.split("\n");
    let holder = d3.select("#viz").append("svg:svg")
        .attr("width", svgwidth)
        .attr("height", svgheight)
        .attr("id", "thesvg");
    let shape = holder.append(linearr[0].trim())
        .attr("id", "theshape");
    transformables = JSON.parse(linearr[linearr.length - 1]);
    linearr.slice(1, -1).forEach(line => {
        parseLn(line, shape);
    });
    return;
}
// todo - x canvas default width side
// todo - validate ranges for coords?
// todo  - transform rectangle - originally top right corner
// todo - make svg square? spares differentiating between w and h, on the other hand no point if svg size is gonna be customizable...
// todo - rearrange order of lines (last?)
// todo - x canvas default width side
// todo - validate ranges for coords?
// todo  - transform rectangle - originally top right corner
// todo - make svg square? spares differentiating between w and h, on the other hand no point if svg size is gonna be customizable...
// todo - rearrange order of lines (last?)