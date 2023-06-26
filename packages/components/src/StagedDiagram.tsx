import { PathResolver } from "@penrose/core";
import { useState } from "react";
import styled from "styled-components";
import { Simple } from "./Simple.js";
import Logo from "./icons/Logo.js";
import Resample from "./icons/Resample.js";

const Container = styled.div`
  position: relative;
  border-radius: 10px;
  border: 0.5px solid rgba(0, 0, 0, 0.2);
  box-shadow: 0 5px 8px 0 rgba(0, 0, 0, 0.2);
  background-color: #fff;
  overflow: hidden;
  min-height: 320px;
`;

const StartOverlay = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
  position: absolute;
  background-color: #0001;
  font-size: 20px;
  color: #000;
  left: 0;
  top: 0;
  right: 0;
  bottom: 0;
  cursor: pointer;
`;

const Stage = styled.div<{ $active?: boolean }>`
  border-radius: 5px;
  background-color: ${(props) => (props.$active ? "#40b4f7" : "#bbb")};
  padding: 1px 3px;
  align-self: start;
  color: white;
`;

const StageContainer = styled.div`
  display: flex;
  margin-right: auto;
  font-family: "Open Sans", sans-serif;
  font-size: 14px;
  width: 100%;
  flex-wrap: wrap;
  justify-content: center;
  margin: 10px 0px;
`;

// variation from SimpleProps is just the initial variation; the actual
// variation is stored in state, can be changed by resampling
export default (props: {
  trio: { substance: string; domain: string; style: string; variation: string };
  imageResolver: PathResolver;
}) => {
  const { trio, imageResolver } = props;
  const { variation, substance, style, domain } = trio;
  const [currVariation, setVariation] = useState(variation);
  const [stageIdx, setStageIdx] = useState(0);
  const [start, setStart] = useState(false);
  const [stages, setStages] = useState([""]);

  return (
    <Container>
      {start ? (
        <>
          <Simple
            name={"embed"}
            domain={domain}
            substance={substance}
            style={style}
            variation={currVariation}
            interactive={false}
            animate={true}
            stepSize={5}
            imageResolver={imageResolver}
            onFrame={(s) => {
              setStages(s.optStages);
              setStageIdx(s.currentStageIndex);
            }}
          />
          <StageContainer>
            {stages.map((stage, n) => {
              const s = stage === "" ? "default" : stage;
              return n === stageIdx ? (
                <Stage $active>{s}</Stage>
              ) : (
                <Stage>{s}</Stage>
              );
            })}
            <div
              onClick={() => setVariation(Math.random().toString())}
              style={{ cursor: "pointer" }}
            >
              <Resample size={28} color={"black"} />
            </div>
          </StageContainer>
        </>
      ) : (
        <div
          style={{
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
          }}
        >
          <StartOverlay onClick={() => setStart(true)}>
            Click to lay out the diagram
          </StartOverlay>
          <Logo width={350} color={"#0001"}></Logo>
        </div>
      )}
    </Container>
  );
};
