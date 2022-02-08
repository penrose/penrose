import { PenroseState, RenderStatic } from "@penrose/core";
import * as React from "react";
import { useEffect, useRef, useState } from "react";
import styled from "styled-components";
import IViewProps from "./IViewProps";

const TimelineStyled = styled.ul`
  background-color: rgba(0, 0, 0, 0.05);
  overflow-x: auto;
  overflow-y: hidden;
  display: flex;
  flex-shrink: 0;
  align-items: center;
  flex-wrap: nowrap;
  box-sizing: border-box;
  margin: 0;
  padding-left: 0.5em;
  padding-right: 2em;
  width: 100%;
  list-style-type: none;
`;

const TimelineItem = styled.li<any>`
  display: inline-block;
  cursor: pointer;
  width: 30px;
  height: 26px;
  flex-shrink: 0;
  border: ${({ selected }: any) => (selected ? "3px" : "1px")} solid
    ${({ selected }: any) => (selected ? "#40B4F7" : "gray")};
  margin-left: 0.5em;
  margin-top: 5px;
  margin-bottom: 5px;
  border-radius: 5px;
  overflow: hidden;
  box-shadow: rgba(0, 0, 0, 0.2) 0px 2px 3px 0px;
`;

function Timeline({ frameIndex, history, selectFrame }: IViewProps) {
  const ref = useRef<any>(null);
  useEffect(() => {
    if (history.length !== history.length && ref.current !== null) {
      ref.current.scrollLeft = ref.current.scrollWidth;
    }
  }, [history]);
  // HACK: stateful due to asynchronicity (could probably do it another way)

  return (
    <TimelineStyled ref={ref}>
      {history.map((frame: PenroseState, k: number) => {
        const [shapeHTML, setShapeHTML] = useState("");
        useEffect(() => {
          (async () => {
            const shape = await RenderStatic(frame, async () => undefined);
            setShapeHTML(shape.outerHTML);
          })();
        }, []);
        return (
          <TimelineItem
            selected={k === frameIndex}
            key={k}
            onClick={() => selectFrame(k)}
            dangerouslySetInnerHTML={{
              __html: shapeHTML,
            }}
          />
        );
      })}
    </TimelineStyled>
  );
}

export default Timeline;
