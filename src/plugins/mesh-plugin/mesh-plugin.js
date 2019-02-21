const fs = require('fs');

function makeSub(json) {
    let newSub = "Edge e\ne := MkEdge(i, j)";
    return newSub;
}

function main() {
   console.log("starting mesh plugin");

   // https://stackabuse.com/reading-and-writing-json-files-with-node-js/
   let rawdata = fs.readFileSync('Sub_enduser.json');
   let subJSON = JSON.parse(rawdata);
   console.log("Received JSON", subJSON);

   let newSub = makeSub(subJSON)
   console.log("writing data: ", newSub);
   fs.writeFileSync('Sub_instantiated.sub', newSub);

   console.log("ending mesh plugin")
}

main();
