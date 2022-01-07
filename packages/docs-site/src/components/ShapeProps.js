import React from "react";
import shapeData from "../../mock-shapedata.json";
//  TODO: get default from shapes lib
export default function ShapeProps({ shapeName }) {
  return (
    <table>
      <thead>
        <tr>
          <th>Property</th>
          <th>Type</th>
        </tr>
      </thead>
      <tbody>
        {Object.entries(shapeData[shapeName]).map(([name, type]) => (
          <tr key={name}>
            <td>{name}</td>
            <td>{type}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
