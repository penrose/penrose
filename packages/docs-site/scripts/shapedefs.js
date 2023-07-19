import {
  makeCanvas,
  sampleShape,
  shapeTypes,
  simpleContext,
} from "@penrose/core";
import chalk from "chalk";
import * as fs from "fs";
import { resolve } from "path";

/**
 * Retrieves defintions for all shapes and writes their properties to a JSON
 * file.  If a filename is not provided, the result is written to stdout.
 *
 * @param outFile The output file (optional)
 */
const getShapeDefs = (outFile) => {
  const outShapes = {}; // List of shapes with properties
  const size = 19; // greater than 3*6; see randFloat usage in Samplers.ts

  // Loop over the shapes
  for (const shapeName of shapeTypes) {
    const shapeSample1 = sampleShape(
      shapeName,
      simpleContext("ShapeProps sample 1"),
      makeCanvas(size, size),
    );
    const shapeSample2 = sampleShape(
      shapeName,
      simpleContext("ShapeProps sample 2"),
      makeCanvas(size, size),
    );
    const outThisShapeDef = { sampled: {}, defaulted: {} };
    outShapes[shapeName] = outThisShapeDef;

    // Loop over the properties
    for (const propName in shapeSample1) {
      const sample1Str = JSON.stringify(shapeSample1[propName].contents);
      const sample2Str = JSON.stringify(shapeSample2[propName].contents);

      if (sample1Str === sample2Str) {
        outThisShapeDef.defaulted[propName] = shapeSample1[propName];
      } else {
        outThisShapeDef.sampled[propName] = shapeSample1[propName];
      }
    }
  }

  // Write the shape definition output
  if (outFile === undefined) {
    console.log(JSON.stringify(outShapes, null, 2));
  } else {
    const out = resolve(outFile);
    fs.writeFileSync(out, JSON.stringify(outShapes, null, 2));
    console.log(chalk.green(`Wrote shape definitions to: ${out}`));
  }
};

getShapeDefs(process.argv[2]);
