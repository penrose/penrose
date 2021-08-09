import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
} from "@material-ui/core";
import {
  styled,
  Button,
  TextField,
  Box,
  Drawer,
  Toolbar,
} from "@material-ui/core";
import { SynthesizerSetting } from "@penrose/core";
import React from "react";
import { MultiselectDropdown } from "./MultiselectDropdown";

export interface SettingsProps {
  generateCallback: (
    setting: SynthesizerSetting,
    numPrograms: number,
    sub: string
  ) => void;
}

interface SettingState {
  substance: string;
  setting: SynthesizerSetting | undefined;
  numPrograms: number;
}

const InputContainer = styled(Box)({
  padding: "0.5rem",
  paddingTop: "1rem",
  width: "25vw",
});

const SettingsDrawer = styled(Drawer)(({ theme }) => ({
  width: "25vw",
  flexShrink: 0,
  overflow: "auto",
  zIndex: theme.zIndex.appBar - 10,
  display: "flex",
}));

// const Btn = styled.button`
//   display: inline-block;
//   color: gray;
//   font-size: 1rem;
//   margin: 1rem;
//   height: 2rem;
//   padding: 0.25rem 1rem;
//   border: 2px solid gray;
//   border-radius: 0.25rem;
//   display: flex;
//   justify-content: center;
//   align-items: center;
// `;

export class Settings extends React.Component<SettingsProps, SettingState> {
  constructor(props: SettingsProps) {
    super(props);
    this.state = { substance: "", setting: undefined, numPrograms: 10 };
  }

  componentDidMount() {
    fetch("public/files/sub_example.txt")
      .then((r) => r.text())
      .then((text) => {
        this.updateSubstance(text);
      });
    fetch("public/files/defaultSetting.json")
      .then((r) => r.json())
      .then((text) => {
        this.updateSetting(text);
      });
  }

  updateSubstance = (newSub: string) => {
    this.setState({
      ...this.state,
      substance: newSub,
    });
  };

  updateSetting = (newSetting: SynthesizerSetting) => {
    this.setState({ setting: newSetting });
    // console.log(Object.entries(this.state.setting).map([k, v]) => {})
  };

  onChange = (event: any) => {
    event.preventDefault();
    if (event.target.name === "sub") {
      console.log("changing substance!", event.target.value);
      this.updateSubstance(event.target.value);
      // } else {
      //   console.log("changing ", event.target.name, event.target.value);
      //   const typeSelect = (s: string, op: any, arr: any[]) => {
      //     if (s === "Type") return { ...op, type: arr };
      //     if (s === "Constructor") return { ...op, constructor: arr };
      //     if (s === "Function") return { ...op, function: arr };
      //     if (s === "Predicate") return { ...op, predicate: arr };
      //   };
      //   const [op, stmtType] = event.target.name.split("-");
      //   const val = event.target.value.replace(/\s/g, "").split(",");
      //   let newSetting = this.state.setting;
      //   console.log(op);
      //   if (newSetting) {
      //     switch (op) {
      //       case "Add":
      //         newSetting = {
      //           ...newSetting,
      //           add: typeSelect(stmtType, newSetting.add, val),
      //         };
      //         break;
      //       case "Delete":
      //         newSetting = {
      //           ...newSetting,
      //           delete: typeSelect(stmtType, newSetting.delete, val),
      //         };
      //         break;
      //       case "Edit":
      //         newSetting = {
      //           ...newSetting,
      //           edit: typeSelect(stmtType, newSetting.edit, val),
      //         };
      //         break;
      //       default:
      //         break;
      //     }
      //     console.log(newSetting);
      //     this.setState({ setting: newSetting });
      //   }
    }
  };

  onGenerateClick = () => {
    console.log("clicky", this.state.numPrograms);
    if (this.state.setting)
      this.props.generateCallback(
        this.state.setting,
        this.state.numPrograms,
        this.state.substance
      );
  };

  onChangeMultiselect = (op: string, stmtType: string) => (
    selected: string[]
  ) => {
    const typeSelect = (s: string, op: any, arr: any[]) => {
      if (s === "Type") return { ...op, type: arr };
      if (s === "Constructor") return { ...op, constructor: arr };
      if (s === "Function") return { ...op, function: arr };
      if (s === "Predicate") return { ...op, predicate: arr };
    };
    let newSetting = this.state.setting;
    if (newSetting) {
      switch (op) {
        case "Add":
          newSetting = {
            ...newSetting,
            add: typeSelect(stmtType, newSetting.add, selected),
          };
          break;
        case "Delete":
          newSetting = {
            ...newSetting,
            delete: typeSelect(stmtType, newSetting.delete, selected),
          };
          break;
        case "Edit":
          newSetting = {
            ...newSetting,
            edit: typeSelect(stmtType, newSetting.edit, selected),
          };
          break;
        default:
          break;
      }
      console.log(newSetting);
      this.setState({ setting: newSetting });
    }
  };

  getDefaults = (mutationType: string, stmtType: string): string[] => {
    let defaults: string[] = [];
    if (this.state.setting) {
      Object.entries(this.state.setting).map(([key, values]: [string, any]) => {
        if (mutationType.toLowerCase() === key) {
          Object.entries(values).map(([k, v]: [string, any]) => {
            if (stmtType.toLowerCase() === k) {
              defaults = v;
            }
          });
        }
      });
    }
    console.log(defaults, this.state.setting);
    return defaults;
  };

  inputElements = () => {
    return ["Add", "Edit", "Delete"].map((op) => (
      <Accordion key={op}>
        <AccordionSummary>{`Configure Statements to ${op}:`}</AccordionSummary>
        <AccordionDetails>
          <InputContainer>
            {["Type", "Constructor", "Function", "Predicate"].map(
              (stmtType) => (
                <MultiselectDropdown
                  stmtType={stmtType}
                  mutationType={op}
                  key={`${op}-${stmtType}`}
                  // name={`${op}-${stmtType}`}
                  onChange={this.onChangeMultiselect(op, stmtType)}
                  defaults={this.getDefaults(op, stmtType)}
                />
              )
            )}
          </InputContainer>
        </AccordionDetails>
      </Accordion>
    ));
  };

  render() {
    return (
      <SettingsDrawer variant="permanent">
        {/* NOTE: Toolbar is used exclusively to space the content underneath the header of the page.
        The Settings element is floating so it must be included */}
        <Toolbar />
        <form
          onSubmit={(e) => {
            e.preventDefault();
            console.log("blocked submit");
          }}
          autoComplete="off"
        >
          <InputContainer>
            <TextField
              rows={10}
              name="sub"
              multiline
              label="Substance Program:"
              variant="outlined"
              fullWidth
              onChange={this.onChange}
              value={this.state.substance}
            />
          </InputContainer>
          <br />
          {this.inputElements()}
          <Button
            onClick={this.onGenerateClick}
            color="secondary"
            variant="outlined"
          >
            Generate Diagrams
          </Button>
        </form>
      </SettingsDrawer>
    );
  }
}
