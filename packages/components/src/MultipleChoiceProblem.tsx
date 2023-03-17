import { useState } from "react";
import Latex from "react-latex-next";
import styled from "styled-components";
import { Simple } from "./Simple";

type DiagramOption = {
  style: string;
  domain: string;
  substance: string;
  variation: string;
  answer: boolean;
};

export interface MultiChoiceProblemProps {
  diagrams: DiagramOption[];
  correctIndices: number[];
  prompt: string;
}

const Content = styled.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
  border-width: 1px;
  background-color: #f5f5f5;
  border-radius: 5px;
  border-style: solid;
  width: 44rem;
`;

const OptionContainer = styled.div<{
  checked: boolean;
  showAnswer: boolean;
  answer: boolean;
}>`
  margin: 0.5rem;
  width: 20rem;
  height: 20rem;
  border-color: ${(props) => (props.checked ? "#40b4f7" : props.theme.primary)};
  border-width: ${(props) => (props.checked ? "2px" : "0.5px")};
  border-style: solid;
  border-radius: 5px;
  background-color: ${(props) =>
    props.showAnswer ? (props.answer ? "#C0F09D40" : "#F69E9E40") : "#fff"};
  display: flex;
  flex-direction: row;
`;

const Index = styled.div`
  font-family: "Roboto Mono";
  padding: 0.5rem;
`;

const Prompt = styled.div`
  font-family: "Open Sans";
  width: 100%;
  text-align: center;
  padding: 1rem;
`;

const Submit = styled.button`
  font-family: "Open Sans";
  margin: 1rem;
  border-width: 0.5px;
  border-style: solid;
  border-radius: 5px;
  margin: 1em;
  padding: 0.25em 1em;
`;

const ProblemChoice = ({
  domain,
  style,
  substance,
  variation,
  index,
  answer,
  showAnswer,
  onSelect,
  onDeselect,
}: {
  domain: string;
  style: string;
  variation: string;
  substance: string;
  index: number;
  answer: boolean;
  showAnswer: boolean;
  onSelect: (index: number) => void;
  onDeselect: (index: number) => void;
}) => {
  const [checked, setChecked] = useState(false);
  return (
    <OptionContainer
      checked={checked}
      answer={answer}
      showAnswer={showAnswer}
      onClick={() => {
        setChecked((c) => {
          if (!c) {
            onSelect(index);
          } else {
            onDeselect(index);
          }
          return !c;
        });
      }}
    >
      <Index>{index + 1}</Index>
      <Simple
        key={`choice-${index}`}
        name={`choice-${index}`}
        domain={domain}
        style={style}
        substance={substance}
        variation={variation}
        interactive={false}
      />
    </OptionContainer>
  );
};

export default function (props: MultiChoiceProblemProps) {
  const [response, setResponse] = useState(new Set());
  const [showAns, setShowAns] = useState(false);
  const diagrams = props.diagrams.map(
    ({ domain, style, substance, variation, answer }, i) => (
      <ProblemChoice
        domain={domain}
        style={style}
        substance={substance}
        variation={variation}
        index={i}
        answer={answer}
        showAnswer={showAns}
        onSelect={(i) => {
          setResponse((prevRes) => prevRes.add(i));
          setShowAns(false);
        }}
        onDeselect={(i) =>
          setResponse((prevRes) => {
            prevRes.delete(i);
            setShowAns(false);
            return prevRes;
          })
        }
      />
    )
  );
  return (
    <Content>
      <Prompt>
        <Latex delimiters={[{ right: "$", left: "$", display: false }]}>
          {props.prompt}
        </Latex>
      </Prompt>

      {diagrams}
      <Submit onClick={() => setShowAns(true)}>Check Answer</Submit>
    </Content>
  );
}
