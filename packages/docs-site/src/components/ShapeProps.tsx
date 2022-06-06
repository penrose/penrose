import React from "react";
import shapeDefs from "../../static/shapeDefs.json";

/**
 * Shows the default and sampled properties of a shape type
 *
 * @param shapeName Name of the shape
 * @returns React element showing the shape's properties
 */
export default function ShapeProps({ shapeName }) {
  const thisShape = shapeDefs[shapeName];
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
        {Object.keys(thisShape["defaulted"]).map((propName) => (
          <tr key={propName}>
            <td>{propName}</td>
            <td>{thisShape["defaulted"][propName].tag}</td>
            <td>{showValue(thisShape["defaulted"][propName])}</td>
          </tr>
        ))}
        {Object.keys(thisShape["sampled"]).map((propName) => (
          <tr key={propName}>
            <td>{propName}</td>
            <td>{thisShape["sampled"][propName].tag}</td>
            <td>
              <span style={{ fontStyle: "italic" }}>sampled</span>
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

/**
 * Shows the value of a shape property.
 *
 * @param propValue The shape's property value
 * @returns React element representing the value
 */
const showValue = (propValue: { contents: any; tag: string }) => {
  const contents = propValue.contents;

  if (typeof contents === "object") {
    switch (contents.tag) {
      case "NONE":
        return "NONE";
      case "RGBA": {
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
    } // switch: contents.tag
    if ("val" in contents) {
      return JSON.stringify(contents.val);
    }
  } // if: object
  return JSON.stringify(contents);
};
