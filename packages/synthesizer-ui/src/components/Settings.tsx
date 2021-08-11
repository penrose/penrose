import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Divider,
  Slider,
  Typography,
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

const DEFAULT_MUTATION_COUNT = [1, 4];

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

const ButtonContainer = styled("div")({
  display: "flex",
  justifyContent: "center",
  alignItems: "center",
  padding: "1rem 0 2rem 0",
});

const SliderDiv = styled("div")({
  padding: "0.5rem",
  paddingBottom: "0",
});

const SliderLabel = styled(Typography)({
  color: "gray",
  fontSize: "1rem",
});

const AccordionHeaderStyled = styled(AccordionSummary)(({ theme }) => ({
  borderColor: theme.palette.primary.light,
  borderWidth: "1px",
  borderStyle: "outset",
  color: theme.palette.primary.main,
  borderRadius: "5px",
}));

const AccordionBodyStyled = styled(AccordionDetails)(({ theme }) => ({
  borderColor: theme.palette.primary.light,
  borderWidth: "1px",
  borderStyle: "outset",
  borderRadius: "3px",
  borderTop: "0px solid black",
}));

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
        this.setState({ setting: text });
      });
  }

  updateSubstance = (newSub: string) => {
    this.setState({
      ...this.state,
      substance: newSub,
    });
  };

  onTextAreaChange = (event: any) => {
    event.preventDefault();
    if (event.target.name === "sub") {
      this.updateSubstance(event.target.value);
    }
  };

  onGenerateClick = () => {
    if (this.state.setting)
      this.props.generateCallback(
        this.state.setting,
        this.state.numPrograms,
        this.state.substance
      );
  };

  onMutationCountChange = (event: any, newValue: number | number[]) => {
    if (this.state.setting)
      this.setState({
        setting: {
          ...this.state.setting,
          mutationCount: newValue as [number, number],
        },
      });
    console.log(this.state.setting?.mutationCount);
  };

  onProgCountChange = (event: any, newValue: number | number[]) => {
    this.setState({ numPrograms: newValue as number });
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
    return defaults;
  };

  inputElements = () => {
    return ["Add", "Edit", "Delete"].map((op) => (
      <Accordion key={op} elevation={0}>
        <AccordionHeaderStyled>{`${op} Statements`}</AccordionHeaderStyled>
        <AccordionBodyStyled>
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
        </AccordionBodyStyled>
      </Accordion>
    ));
  };

  render() {
    return (
      <SettingsDrawer variant="permanent">
        {/* NOTE: Toolbar is used exclusively to space the content underneath the header of the page.
        The Settings element is floating so it must be included */}
        <Toolbar />
        <InputContainer>
          <TextField
            rows={10}
            name="sub"
            multiline
            label="Substance Program:"
            variant="outlined"
            fullWidth
            onChange={this.onTextAreaChange}
            value={this.state.substance}
          />
          <SliderDiv>
            <SliderLabel>Diagrams to generate:</SliderLabel>
            <Slider
              valueLabelDisplay="auto"
              step={1}
              marks={[
                { value: 1, label: "1" },
                { value: 10, label: "10" },
                { value: 20, label: "20" },
              ]}
              value={this.state.numPrograms}
              min={1}
              max={20}
              onChange={this.onProgCountChange}
            />
          </SliderDiv>
          <SliderDiv>
            <SliderLabel>Mutations/program:</SliderLabel>
            <Slider
              valueLabelDisplay="auto"
              step={1}
              marks={[
                { value: 1, label: "1" },
                { value: 5, label: "5" },
              ]}
              value={
                this.state.setting
                  ? this.state.setting.mutationCount
                  : DEFAULT_MUTATION_COUNT
              }
              min={1}
              max={5}
              onChange={this.onMutationCountChange}
            />
          </SliderDiv>
        </InputContainer>
        <br />
        <InputContainer>{this.inputElements()}</InputContainer>
        <ButtonContainer>
          <Button
            onClick={this.onGenerateClick}
            color="primary"
            variant="contained"
          >
            Generate Diagrams
          </Button>
        </ButtonContainer>
      </SettingsDrawer>
    );
  }
}
