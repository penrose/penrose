import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
  Button,
  Drawer,
  MenuItem,
  Select,
  Slider,
  styled,
  TextField,
  Toolbar,
  Typography,
} from "@material-ui/core";
import { Listing } from "@penrose/components";
import {
  compileDomain,
  Env,
  showError,
  SynthesizerSetting,
} from "@penrose/core";
import { examples } from "@penrose/examples";
import React from "react";
import { Preset, presets } from "../examples";
import { MultiselectDropdown } from "./MultiselectDropdown";

const DEFAULT_MUTATION_COUNT = [1, 4];

interface StmtType {
  tag: string;
  values: string[];
}

interface PartialEnv {
  types: StmtType;
  constructors: StmtType;
  functions: StmtType;
  predicates: StmtType;
}

const defaultEnv: PartialEnv = {
  types: {
    tag: "Type",
    values: [],
  },
  constructors: {
    tag: "Constructor",
    values: [],
  },
  functions: {
    tag: "Function",
    values: [],
  },
  predicates: {
    tag: "Predicate",
    values: [],
  },
};

export interface SettingsProps {
  generateCallback: (
    setting: SynthesizerSetting,
    numPrograms: number,
    dsl: string,
    sub: string,
    sty: string
  ) => void;
  defaultDomain: string;
  defaultStyle: string;
}

interface SettingState {
  substance: string;
  setting: SynthesizerSetting | undefined;
  numPrograms: number;
  domainEnv: PartialEnv;
  domain: string;
  style: string;
  prompt: string;
  env?: Env;
}

const SettingContainer = styled(Box)({
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

const SettingDiv = styled("div")({
  padding: "0.5rem",
  paddingBottom: "0",
});

const SettingLabel = styled(Typography)({
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
    this.state = {
      substance: examples["geometry-domain"].textbook_problems["c04p01.sub"],
      setting: undefined,
      numPrograms: 10,
      domainEnv: defaultEnv,
      domain: this.props.defaultDomain,
      style: this.props.defaultStyle,
      prompt: "",
    };
  }

  // TODO: current implementation will not update the UI if there is a domain compiler error
  // at any point in time, instead, we should display a helpful error message.
  updateDomainEnv = (newDomain: string) => {
    const result = compileDomain(newDomain);
    if (result.isOk()) {
      this.setState({
        domainEnv: {
          types: {
            tag: "Type",
            values: [...result.value.types.keys()],
          },
          constructors: {
            tag: "Constructor",
            values: [...result.value.constructors.keys()],
          },
          functions: {
            tag: "Function",
            values: [...result.value.functions.keys()],
          },
          predicates: {
            tag: "Predicate",
            values: [...result.value.predicates.keys()],
          },
        },
      });
    } else {
      console.error(showError(result.error));
    }
  };

  componentDidUpdate(prev: SettingsProps) {
    if (
      this.props.defaultDomain !== prev.defaultDomain &&
      this.props.defaultDomain.length > 0
    ) {
      this.updateDomainEnv(this.props.defaultDomain);
      this.setState({ domain: this.props.defaultDomain });
    }
    if (
      this.props.defaultStyle !== prev.defaultStyle &&
      this.props.defaultStyle.length > 0
    ) {
      this.setState({
        style: this.props.defaultStyle,
      });
    }
  }

  onTextAreaChange = (event: any) => {
    event.preventDefault();
    if (event.target.name === "dsl") {
      this.setState({
        domain: event.target.value,
      });
      this.updateDomainEnv(event.target.value);
    } else if (event.target.name === "sty") {
      this.setState({
        style: event.target.value,
      });
    }
  };

  onGenerateClick = () => {
    if (this.state.setting)
      this.props.generateCallback(
        this.state.setting,
        this.state.numPrograms,
        this.state.domain,
        this.state.substance,
        this.state.style
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
      for (const entryIdx in Object.entries(this.state.setting)) {
        const [key, values]: [string, any] = Object.entries(this.state.setting)[
          entryIdx
        ];
        if (mutationType.toLowerCase() === key) {
          for (const valueIdx in Object.entries(values)) {
            const [k, v]: [string, any] = Object.entries(values)[valueIdx];
            if (stmtType.toLowerCase() === k) {
              defaults = v;
            }
          }
        }
      }
    }

    return defaults;
  };

  inputElements = () => {
    return ["Add", "Edit", "Delete"].map((op) => (
      <Accordion key={op} elevation={0}>
        <AccordionHeaderStyled>{`${op} Statements`}</AccordionHeaderStyled>
        <AccordionBodyStyled>
          <SettingContainer>
            {Object.values(this.state.domainEnv).map((stmtType: StmtType) => (
              <MultiselectDropdown
                stmtType={stmtType.tag}
                mutationType={op}
                key={`${op}-${stmtType.tag}`}
                // name={`${op}-${stmtType}`}
                onChange={this.onChangeMultiselect(op, stmtType.tag)}
                defaults={this.getDefaults(op, stmtType.tag)}
                options={stmtType.values}
              />
            ))}
          </SettingContainer>
        </AccordionBodyStyled>
      </Accordion>
    ));
  };

  presets = () =>
    Object.entries(presets).map(([name, { displayName }]: [string, Preset]) => (
      <MenuItem key={name} value={name}>
        {displayName}
      </MenuItem>
    ));

  handlePreset = (key: string) => {
    this.setState({ ...this.state, ...presets[key] });
  };

  componentDidMount = () => {
    this.handlePreset("c04p01");
  };

  render() {
    return (
      <SettingsDrawer variant="permanent">
        {/* NOTE: Toolbar is used exclusively to space the content underneath the header of the page.
        The Settings element is floating so it must be included */}
        <Toolbar />
        <SettingContainer>
          <SettingDiv>
            <SettingLabel>Example preset:</SettingLabel>
            <Select
              key="preset"
              labelId="preset-select-label"
              id="preset-select"
              label="preset"
              defaultValue={"c04p01"}
              onChange={(e) => this.handlePreset(e.target.value as string)}
            >
              {this.presets()}
            </Select>
            <SettingLabel>Prompt:</SettingLabel>
            <div>{this.state.prompt}</div>
          </SettingDiv>
          <br />
          <Accordion key="substance" elevation={0}>
            <AccordionHeaderStyled>{`Substance Program`}</AccordionHeaderStyled>
            <AccordionBodyStyled style={{ padding: 0 }}>
              <Listing
                domain={this.state.domain}
                substance={this.state.substance}
                onChange={(sub: string) =>
                  this.setState({
                    substance: sub,
                  })
                }
                width={"100%"}
                height={"500px"}
                monacoOptions={{ theme: "vs" }}
                readOnly={false}
              />
            </AccordionBodyStyled>
          </Accordion>
          <Accordion key="domain" elevation={0}>
            <AccordionHeaderStyled>{`Domain Program`}</AccordionHeaderStyled>
            <AccordionBodyStyled style={{ padding: 0 }}>
              <TextField
                rows={20}
                name="dsl"
                multiline
                variant="outlined"
                fullWidth
                inputProps={{ style: { fontSize: ".8rem" } }}
                style={{ padding: 0 }}
                onChange={this.onTextAreaChange}
                value={this.state.domain}
              />
            </AccordionBodyStyled>
          </Accordion>
          <Accordion key="style" elevation={0}>
            <AccordionHeaderStyled>Style Program</AccordionHeaderStyled>
            <AccordionBodyStyled style={{ padding: 0 }}>
              <TextField
                rows={20}
                name="sty"
                multiline
                fullWidth
                variant="outlined"
                style={{ padding: 0 }}
                inputProps={{
                  style: { fontSize: ".8rem", overflow: "scroll" },
                }}
                onChange={this.onTextAreaChange}
                value={this.state.style}
              />
            </AccordionBodyStyled>
          </Accordion>
          <SettingDiv>
            <SettingLabel>Diagrams to generate:</SettingLabel>
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
          </SettingDiv>
          <SettingDiv>
            <SettingLabel>Mutations/program:</SettingLabel>
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
          </SettingDiv>
        </SettingContainer>
        <br />
        <SettingContainer>{this.inputElements()}</SettingContainer>
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
