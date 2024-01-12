import React, { useState } from "react";
import styled from "styled-components";

interface Segment {
  label: string;
  steps: number;
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
  if (stages.length === 0) return null;
  // compute the step ranges for each stage
  const stageRanges = stages.reduce(
    (acc, stage, i) => [
      ...acc,
      {
        start: i === 0 ? 0 : acc[i - 1].end,
        end: i === 0 ? stage.steps : acc[i - 1].end + stage.steps,
      },
    ],
    [] as { start: number; end: number }[],
  );

  const totalSteps = stageRanges[stageRanges.length - 1].end;
  const [value, setValue] = useState<number>(totalSteps - 1);
  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = parseInt(e.target.value, 10);
    setValue(newValue);
    onChange(newValue);
  };

  return (
    <SliderContainer>
      <Slider
        disabled={disabled}
        type="range"
        min="0"
        max={totalSteps - 1}
        value={value}
        onChange={handleChange}
      />
      <div>
        {stages.map((stage, index) => (
          <StageLabel
            key={index}
            enabled={stageRanges[index].start <= value}
            width={(stage.steps / totalSteps) * 100}
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
