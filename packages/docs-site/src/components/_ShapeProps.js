import { makeCanvas, shapedefs, simpleContext } from "@penrose/core";
import React from "react";

/* HACK to see if it's a sampled or clamped value */
const showValue = (prop, def1, def2) => {
  const def1Str = JSON.stringify(def1[prop].contents);
  const def2Str = JSON.stringify(def2[prop].contents);
  const contents = def1[prop].contents;
  if (def1Str === def2Str && contents.tag !== "NONE") {
    if (def1[prop].tag === "PtListV") {
      return JSON.stringify(contents.map((pt) => pt.map(({ val }) => val)));
    }
    if (typeof contents === "object") {
      if (contents.tag === "RGBA") {
        const arr = contents.contents;
        return (
          <div>
            <div
              style={{
                display: "inline",
                border: "1px solid gray",
                backgroundColor: `rgba(
                  ${arr[0] * 255}, 
                  ${arr[1] * 255}, 
                  ${arr[2] * 255}, 
                  ${arr[3]})`,
                borderRadius: "5px",
              }}
            >
              &nbsp;&nbsp;&nbsp;&nbsp;
            </div>
            <span style={{ marginLeft: "1em" }}>
              {`rgba(${arr[0]},${arr[1]},${arr[2]},${arr[3]})`}
            </span>
          </div>
        );
      }
      if ("val" in contents) {
        return JSON.stringify(contents.val);
      }
      console.log(def1[prop]);
    }
    return def1Str;
  }
  return <span style={{ fontStyle: "italic" }}>sampled</span>;
};

export default function _ShapeProps({ shapeName }) {
  const shapedef = shapedefs[shapeName];
  // HACK from main repo
  const size = 19; // greater than 3*6; see randFloat usage in Samplers.ts
  const def = shapedef.sampler(
    simpleContext("_ShapeProps def"),
    makeCanvas(size, size)
  );
  const def_sample = shapedef.sampler(
    simpleContext("_ShapeProps def_sample"),
    makeCanvas(size, size)
  );
  return (
    <table>
      <thead>
        <tr>
          <th>Property</th>
          <th>Type</th>
          <th>Default</th>
        </tr>
      </thead>
      <tbody>
        {Object.entries(def).map(([prop, sampler]) => (
          <tr key={prop}>
            <td>{prop}</td>
            <td>{sampler.tag}</td>
            <td>{showValue(prop, def, def_sample)}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
