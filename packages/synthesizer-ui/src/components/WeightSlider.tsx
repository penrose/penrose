import * as _ from "lodash";
import React, { useRef } from "react";
import styled from "styled-components";

const BAR_HEIGHT = 3;
const THUMB_WIDTH = 0.9;
const THUMB_BORDER = 0.2;

const SlideBar = styled.div`
  display: flex;
  width: 100%;
  border-radius: 3px;
  height: ${BAR_HEIGHT}rem;
  border-radius: 10px;
  overflow: hidden;
`;
const Rect = styled.div`
  height: ${BAR_HEIGHT}rem;
  overflow: hidden;
  text-align: center;
  display: flex;
  flex-direction: column;
  justify-content: center;
  .text {
    font-weight: 600;
    font-size: 0.9rem;
  }
  .percentage {
    font-size: 0.83rem;
  }
`;

const SlideThumb = styled.div`
  border-left: ${THUMB_BORDER}rem;
  border-right: ${THUMB_BORDER}rem;
  border-top: 0;
  border-bottom: 0;
  border-style: solid;
  padding: 0;
  width: ${THUMB_WIDTH}rem;
  height: ${BAR_HEIGHT}rem;
  top: 0px;
  background: none;
  cursor: ew-resize;
  user-select: none;
`;

const remToPixels = (rem: number) => {
  return rem * parseFloat(getComputedStyle(document.documentElement).fontSize);
};

interface Division {
  text: string;
  color: string;
  percentage: number;
}

interface ThumbProps {
  barRef: any;
  divisions: Division[];
  leftIndex: number;
  leftColor: string;
  rightColor: string;
  setDivisions: (divs: Division[]) => void;
  thumbsSpace: number;
}

const Thumb = ({
  barRef,
  divisions,
  leftIndex,
  leftColor,
  rightColor,
  setDivisions,
  thumbsSpace,
}: ThumbProps): JSX.Element => {
  const rightIndex = leftIndex + 1;
  let oldX = 0;
  let oldPercentages = { left: 0, right: 0 };

  const handleMouseMove = (event: { clientX: number }) => {
    const newX = event.clientX;
    const barWidth = barRef.current.offsetWidth;

    let newPercentages = _.clone(oldPercentages);

    const changePercentage =
      ((oldX - newX) / (barWidth - remToPixels(thumbsSpace))) * 100;

    if (oldPercentages.left - changePercentage < 0) {
      newPercentages.right =
        oldPercentages.right + Math.abs(oldPercentages.left);
      newPercentages.left = 0;
    } else if (oldPercentages.right + changePercentage < 0) {
      newPercentages.left =
        oldPercentages.left + Math.abs(oldPercentages.right);
      newPercentages.right = 0;
    } else {
      newPercentages.left = oldPercentages.left - changePercentage;
      newPercentages.right = oldPercentages.right + changePercentage;
    }

    let tempDivisions = divisions;
    tempDivisions[leftIndex].percentage = newPercentages.left;
    tempDivisions[rightIndex].percentage = newPercentages.right;

    setDivisions(_.clone(tempDivisions));
  };

  const handlePointerUp = () => {
    document.removeEventListener("pointermove", handleMouseMove);
    document.removeEventListener("pointerup", handlePointerUp);
  };

  const handlePointerDown = (event: { clientX: number }) => {
    oldX = event.clientX;
    oldPercentages.left = divisions[leftIndex].percentage;
    oldPercentages.right = divisions[rightIndex].percentage;

    document.addEventListener("pointermove", handleMouseMove);
    document.addEventListener("pointerup", handlePointerUp);
  };

  return (
    <SlideThumb
      onPointerDown={handlePointerDown}
      style={{ borderLeftColor: leftColor, borderRightColor: rightColor }}
    />
  );
};

const PercentagesSlider = ({
  divisions,
  setDivisions,
}: {
  divisions: Division[];
  setDivisions: any;
}) => {
  const thumbsSpace = (divisions.length - 1) * THUMB_WIDTH;
  const barRef: any = useRef();
  return (
    <SlideBar ref={barRef}>
      {divisions.map((division, index) => (
        <React.Fragment key={index}>
          <Rect
            style={{
              width: `calc((100% - ${thumbsSpace}rem) * ${division.percentage} / 100 )`,
              backgroundColor: division.color,
            }}
          >
            <span className="text" style={{ color: "#fff" }}>
              {division.text}
            </span>
            <span className="percentage" style={{ color: "#fff" }}>
              {division.percentage.toFixed(0)}%
            </span>
          </Rect>
          {index < divisions.length - 1 ? (
            <Thumb
              barRef={barRef}
              divisions={divisions}
              leftIndex={index}
              leftColor={division.color}
              // rightIndex={divisions[index + 1].percentage}
              rightColor={divisions[index + 1].color}
              setDivisions={setDivisions}
              thumbsSpace={thumbsSpace}
            />
          ) : (
            <React.Fragment></React.Fragment>
          )}
        </React.Fragment>
      ))}
    </SlideBar>
  );
};

export default PercentagesSlider;
