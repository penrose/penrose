import { useState } from "react";
import styled from "styled-components";
import { Simple, SimpleProps } from "./Simple.js";
import Resample from "./icons/Resample.js";

const Container = styled.div`
  position: relative;
  border-radius: 10px;
  border: 0.5px solid rgba(0, 0, 0, 0.2);
  box-shadow: 0 5px 8px 0 rgba(0, 0, 0, 0.2);
  overflow: hidden;
`;

const Stage = styled.div<{ $active?: boolean }>`
  border-radius: 5px;
  background-color: ${(props) => (props.$active ? "#40b4f7" : "#bbb")};
  padding: 3px 5px;
  align-self: start;
  color: white;
`;

const StageContainer = styled.div`
  display: flex;
  margin-right: auto;
  font-family: "Open Sans", sans-serif;
  width: 100%;
  justify-content: center;
  margin-bottom: 10px;
`;

// variation from SimpleProps is just the initial variation; the actual
// variation is stored in state, can be changed by resampling
export default ({
  variation,
  substance,
  style,
  domain,
  imageResolver,
}: SimpleProps) => {
  const [currVariation, setVariation] = useState(variation);
  const [stageIdx, setStageIdx] = useState(0);
  const [stages, setStages] = useState([""]);

  return (
    <Container>
      <Simple
        name={"embed"}
        domain={domain}
        substance={substance}
        style={style}
        variation={currVariation}
        interactive={false}
        animate={true}
        imageResolver={imageResolver}
        onFrame={(s) => {
          setStages(s.optStages);
          setStageIdx(s.currentStageIndex);
        }}
      />
      <StageContainer>
        {stages.map((s, n) =>
          n === stageIdx ? <Stage $active>{s}</Stage> : <Stage>{s}</Stage>
        )}
        <div onClick={() => setVariation(Math.random().toString())}>
          <Resample size={28} color={"black"} />
        </div>
      </StageContainer>
    </Container>
  );
};
