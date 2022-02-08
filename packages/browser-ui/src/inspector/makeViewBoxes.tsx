import { bBoxDims, RenderShape, Shape } from "@penrose/core";
import * as React from "react";
import { useEffect, useState } from "react";
import styled from "styled-components";

// styling for shape inside viewbox - see ShapeView or Mod
export const ShapeItem = styled.li<React.OptionHTMLAttributes<never>>`
  display: block;
  padding: 1em;
  margin-top: -1px;
  border: 1px solid #d1d1d1;
  background-color: ${(selected): string => (selected ? "#F9F9F9" : "#f0f0f0")};
  color: rgba(0, 0, 0, 0.5);
  font-family: monospace;
  display: flex;
  flex-direction: row;
  align-items: center;
  cursor: pointer;
`;

const makeViewBoxes = (
  shapes: Shape[],
  selectedShape: number,
  setSelectedShape: (key: number) => void
): JSX.Element => {
  return (
    <div style={{ overflowY: "auto", height: "100%" }}>
      <ul
        style={{
          listStyleType: "none",
          padding: "0 0 1em 0",
          margin: 0,
          top: 0,
          left: 0,
          right: 0,
        }}
      >
        {shapes.map(({ properties, shapeType }: Shape, key: number) => {
          // If the inspector is crashing around here, then probably the shape doesn't have the width/height properties, so add a special case as below
          // console.log("properties, shapeType", properties, shapeType, properties.w, properties.h);
          const [w, h] = bBoxDims(properties, shapeType);

          // HACK: stateful due to asynchronicity (could probably do it another way)
          const [shapeHTML, setShapeHTML] = useState("");
          useEffect(() => {
            (async () => {
              const shape = await RenderShape({
                shape: { properties, shapeType },
                labels: [],
                canvasSize: [w, h],
                pathResolver: async () => undefined,
              });
              setShapeHTML(shape.outerHTML);
            })();
          }, []);
          return (
            <ShapeItem
              key={`shapePreview-${key}`}
              selected={selectedShape === key}
              onClick={() => setSelectedShape(key)}
            >
              <div>
                <svg
                  viewBox={`0 0 ${w} ${h}`}
                  width="50"
                  height="50"
                  dangerouslySetInnerHTML={{
                    __html: shapeHTML,
                  }}
                />
              </div>
              <div style={{ margin: "0.5em" }}>
                <span>{properties.name.contents}</span>
              </div>
            </ShapeItem>
          );
        })}
      </ul>
    </div>
  );
};

export default makeViewBoxes;
