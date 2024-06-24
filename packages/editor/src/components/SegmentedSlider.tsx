import React, { useEffect, useState } from "react";
import styled from "styled-components";

interface Segment {
  label: string;
  frames: number;
  cumulativeFrames: number;
  color: string;
}

interface StageLabelProps {
  enabled?: boolean;
  width: number;
  color: string;
}

const SliderContainer = styled.div`
  width: 100%;
  padding: 10px;
`;

const Slider = styled.input`
  width: 100%;
  cursor: pointer;
`;

const StageLabel = styled.span<StageLabelProps>`
  display: inline-block;
  width: ${(props) => props.width}%;
  text-align: center;
  font-size: 0.8em;
  font-family: "Roboto Mono", monospace;
  padding-top: 0.2em;
  padding-bottom: 0.2em;
  color: white;
  background-color: ${(props) => (props.enabled ? props.color : "#aaa")};
  border-radius: 5px;
`;

interface SegmentedSliderProps {
  disabled?: boolean;
  segments: Segment[];
  onChange: (index: number) => void;
}

const SegmentedSlider: React.FC<SegmentedSliderProps> = ({
  segments: stages,
  disabled,
  onChange,
}) => {
  // compute the step ranges for each stage
  let stageRanges = stages.map((stage, i) => ({
    start: i === 0 ? 0 : stages[i - 1].cumulativeFrames,
    end: stage.cumulativeFrames,
  }));
  if (stageRanges.length === 0) {
    stageRanges = [{ start: 0, end: 0 }];
  }

  const [dragged, setDragged] = useState<boolean>(false);
  const [value, setValue] = useState<number>(0);
  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setDragged(true);
    const newValue = parseInt(e.target.value, 10);
    setValue(newValue);
    onChange(newValue);
  };

  useEffect(() => {
    if (disabled) {
      setDragged(false);
    }
  }, [disabled]);

  const totalFrames = stageRanges[stageRanges.length - 1].end;
  const maxValue = totalFrames - 1;
  const currValue = dragged ? value : maxValue;

  return (
    <SliderContainer>
      <Slider
        disabled={disabled}
        type="range"
        min="0"
        max={maxValue}
        value={currValue}
        onChange={handleChange}
      />
      <div>
        {stages.map((stage, index) => (
          <StageLabel
            key={index}
            enabled={stageRanges[index].start <= currValue}
            width={(stage.frames / (!!totalFrames ? totalFrames : 1)) * 100}
            color={stage.color}
          >
            {stage.label}
          </StageLabel>
        ))}
      </div>
    </SliderContainer>
  );
};

export default SegmentedSlider;

// Usage Example
// <SegmentedSlider
//   stages={[
//     { label: 'Stage 1', steps: 10, color: 'red' },
//     { label: 'Stage 2', steps: 20, color: 'blue' },
//     { label: 'Stage 3', steps: 30, color: 'green' }
//   ]}
//   onChange={(index) => console.log('Current step index:', index)}
// />
