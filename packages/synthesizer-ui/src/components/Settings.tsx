import { SynthesizerSetting } from "@penrose/core";
import React from "react";
import styled from "styled-components";

export interface SettingsProps {
  updateSettings: (setting: SynthesizerSetting) => {};
  generateCallback: (sub: string) => void;
}

interface SettingState {
  substance: string;
  setting: SynthesizerSetting;
}

const Section = styled.section`
  width: 30vw;
  height: auto;
  display: flex;
  flex-direction: column;
  border-right: 1px solid black;
`;

const SubstanceInput = styled.textarea`
  width: 95%;
  height: 200px;
`;

const InputContainer = styled.section`
  display: flex;
  border-bottom: 1px solid black;
  flex-direction: column;
  justify-content: flex-start;
  padding: 0.45rem;
  padding-bottom: 1rem;
`;

const Btn = styled.button`
  display: inline-block;
  color: gray;
  font-size: 1rem;
  margin: 1rem;
  height: 2rem;
  padding: 0.25rem 1rem;
  border: 2px solid gray;
  border-radius: 0.25rem;
  display: flex;
  justify-content: center;
  align-items: center;
`;

export const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 4],
  numPrograms: 10,
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.2,
  },
  add: {
    type: [],
    function: [],
    constructor: ["InteriorAngle"],
    predicate: ["EqualLength", "RightUnmarked"],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["RightMarked", "EqualLengthMarker", "EqualLength"],
  },
  edit: {
    type: [],
    function: [],
    constructor: ["MkSegment"],
    predicate: ["RightMarked", "EqualLength", "EqualLengthMarker"],
  },
};

const testSub = `
Plane P
Point A,B, C, D, E
In(A, P)
In(B, P)
In(C, P)
In(D, P)
In(E, P)
Segment s1 := MkSegment(A,E)
Segment s2 := MkSegment(E,C)
Segment s3 := MkSegment(A,B)
Segment s4 := MkSegment(B,C)
Segment s5 := MkSegment(C,D)
Segment s6 := MkSegment(D,A)
Segment s8 := MkSegment(E,D)
Collinear(A,E,C)
Collinear(B,E,D) 
Angle r := InteriorAngle(B,E,C)
EqualLength(s1, s2)
EqualLengthMarker1(s1, s2)
RightMarked(r)
`;

export class Settings extends React.Component<SettingsProps, SettingState> {
  constructor(props: SettingsProps) {
    super(props);
    this.state = { substance: testSub, setting: defaultSetting };
  }

  componentDidUpdate() {
    this.props.updateSettings(this.state.setting);
  }

  updateSubstance = (newSub: string) => {
    this.setState({
      ...this.state,
      substance: newSub,
    });
    this.props.updateSettings(this.state.setting);
  };

  onChange = (event: any) => {
    event.preventDefault();
    const target = event.target;
    console.log(target);
  };

  inputElements() {
    const setting = this.state.setting;
    const ops = ["Add", "Edit", "Delete"];
    const types = ["Type", "Constructor", "Function", "Predicate"];
    return [setting.add, setting.delete, setting.edit].map((op, idx) => (
      <InputContainer key={ops[idx]}>
        {`${ops[idx]}:`}
        {[op.type, op.predicate, op.function, op.predicate].map(
          (type: string[], i: number) => (
            <input
              type="text"
              key={`${ops[idx]}-${types[i]}`}
              name={`${ops[idx]}-${types[i]}`}
              placeholder={`${types[i]} Statements`}
              value={type.join(", ")}
              onChange={this.onChange}
            />
          )
        )}
      </InputContainer>
    ));
  }

  render() {
    return (
      <Section>
        <form
          onSubmit={(e) => {
            e.preventDefault();
            console.log("blocked submit");
          }}
        >
          <InputContainer>
            Substance:
            <SubstanceInput
              name="sub"
              onChange={this.onChange}
              value={testSub}
            />
          </InputContainer>
          <br />
          {this.inputElements()}
          <Btn
            onClick={() => this.props.generateCallback(this.state.substance)}
          >
            Generate Diagrams
          </Btn>
        </form>
      </Section>
    );
  }
}
