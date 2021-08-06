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
  height: calc(100vh - 6.75rem);
  overflow: auto;
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

export class Settings extends React.Component<SettingsProps, SettingState> {
  constructor(props: SettingsProps) {
    super(props);
    this.state = { substance: "", setting: defaultSetting };
  }

  componentDidMount() {
    fetch("public/files/sub_example.txt")
      .then((r) => r.text())
      .then((text) => {
        this.updateSubstance(text);
      });
  }

  componentDidUpdate() {
    this.props.updateSettings(this.state.setting);
  }

  updateSubstance = (newSub: string) => {
    this.setState({
      ...this.state,
      substance: newSub,
    });
  };

  updateSetting = (newSetting: SynthesizerSetting) => {
    this.setState({ setting: newSetting });
  };

  onChange = (event: any) => {
    event.preventDefault();
    if (event.target.name === "sub") {
      console.log("changing substance!", event.target.value);
      this.updateSubstance(event.target.value);
    } else {
      console.log("changing ", event.target.name, event.target.value);
      const typeSelect = (s: string, op: any, arr: any[]) => {
        if (s === "Type") return { ...op, type: arr };
        if (s === "Constructor") return { ...op, constructor: arr };
        if (s === "Function") return { ...op, function: arr };
        if (s === "Predicate") return { ...op, predicate: arr };
      };
      const [op, stmtType] = event.target.name.split("-");
      const val = event.target.value.replace(/\s/g, "").split(",");
      let newSetting = this.state.setting;
      console.log(op);
      switch (op) {
        case "Add":
          newSetting = {
            ...newSetting,
            add: typeSelect(stmtType, newSetting.add, val),
          };
          break;
        case "Delete":
          newSetting = {
            ...newSetting,
            delete: typeSelect(stmtType, newSetting.delete, val),
          };
          break;
        case "Edit":
          newSetting = {
            ...newSetting,
            edit: typeSelect(stmtType, newSetting.edit, val),
          };
          break;
        default:
          break;
      }
      console.log(newSetting);
      this.setState({ setting: newSetting });
    }
  };

  onGenerateClick = () => {
    this.props.updateSettings(this.state.setting);
    this.props.generateCallback(this.state.substance);
  };

  inputElements = () => {
    const setting = this.state.setting;
    const ops = ["Add", "Edit", "Delete"];
    const types = ["Type", "Constructor", "Function", "Predicate"];
    return [setting.add, setting.delete, setting.edit].map((op, idx) => (
      <InputContainer key={ops[idx]}>
        {`${ops[idx]}:`}
        {[op.type, op.constructor, op.function, op.predicate].map(
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
  };

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
              value={this.state.substance}
            />
          </InputContainer>
          <br />
          {this.inputElements()}
          <Btn onClick={this.onGenerateClick}>Generate Diagrams</Btn>
        </form>
      </Section>
    );
  }
}
