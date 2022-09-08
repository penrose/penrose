#!/usr/bin/env node

const fs = require("fs");

const [, , filename, big, ...smalls] = process.argv;
const aggregateData = JSON.parse(fs.readFileSync(filename, "utf8"));

const data = [];
const sums = Object.fromEntries(smalls.map((small) => [small, 0]));

for (const [, { timeTaken }] of Object.entries(aggregateData)) {
  const dataPoint = {};
  for (const small of smalls) {
    dataPoint[small] = timeTaken[small] / timeTaken[big];
    sums[small] += dataPoint[small];
  }
  data.push(dataPoint);
}

const means = Object.fromEntries(
  Object.entries(sums).map(([key, value]) => [key, value / data.length])
);

const sumsOfSquares = Object.fromEntries(smalls.map((small) => [small, 0]));

for (const dataPoint of data) {
  for (const small of smalls) {
    sumsOfSquares[small] += (dataPoint[small] - means[small]) ** 2;
  }
}

const stdevs = Object.fromEntries(
  Object.entries(sumsOfSquares).map(([key, value]) => [
    key,
    Math.sqrt(value / data.length),
  ])
);

console.log();
console.log(`Percentages of ${big} time:`);
console.log();
const padSize = Math.max(...smalls.map((small) => small.length));
for (const small of smalls) {
  const name = small.padEnd(padSize);
  const mean = (means[small] * 100).toFixed(2).padStart(5);
  const stdev = (stdevs[small] * 100).toFixed(2).padStart(5);
  console.log(`    ${name} ${mean}% ± ${stdev}%`);
}
