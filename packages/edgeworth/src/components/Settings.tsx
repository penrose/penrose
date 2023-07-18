import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Box,
  Button,
  Drawer,
  InputLabel,
  MenuItem,
  Select,
  Slider,
  Tab,
  Tabs,
  TextField,
  Toolbar,
  Typography,
  styled,
} from "@material-ui/core";
import { Listing } from "@penrose/components";
import { Env, compileDomain, showError } from "@penrose/core";
import c04p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c04p01.substance";
import React from "react";
import Latex from "react-latex-next";
import { Preset, domains, presets } from "../examples.js";
import {
  DeclTypes,
  MatchSetting,
  SynthesizerSetting,
} from "../synthesis/Synthesizer.js";
import { wildcardType } from "../util.js";
import { MultiselectDropdown } from "./MultiselectDropdown.js";
import WeightSlider from "./WeightSlider.js";

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

// derived from https://mui.com/material-ui/react-tabs/
interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  currentTab: number;
}

export interface SettingsProps {
  generateCallback: (
    setting: SynthesizerSetting,
    seed: string,
    numPrograms: number,
    dsl: string,
    prompt: string,
    sty: string,
    llmInput: string,
    currentTab: number,
    domainSelect: string,
    presetSelect: string,
  ) => void;
  onPrompt: (prompt: string) => void;
  defaultDomain: string;
  defaultStyle: string;
}

interface SettingState {
  substance: string;
  setting: SynthesizerSetting | undefined;
  seed: string;
  numPrograms: number;
  domainEnv: PartialEnv;
  domain: string;
  style: string;
  prompt: string;
  llmInput: string;
  llmRunning: boolean;
  currentTab: number;
  domainSelect: string;
  presetSelect: string;
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

// derived from https://mui.com/material-ui/react-tabs/
const TabPanel = ({ index, currentTab, children }: TabPanelProps) => {
  return (
    <div hidden={currentTab !== index}>
      {currentTab === index && <>{children}</>}
    </div>
  );
};

export class Settings extends React.Component<SettingsProps, SettingState> {
  constructor(props: SettingsProps) {
    super(props);
    this.state = {
      substance: c04p01,
      setting: undefined,
      numPrograms: 10,
      seed: "test0", // default seed
      domainEnv: defaultEnv,
      domain: this.props.defaultDomain,
      style: this.props.defaultStyle,
      prompt: "",
      llmInput: "",
      llmRunning: false,
      currentTab: 0,
      domainSelect: "moleculesDomain",
      presetSelect: "lewis_0",
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
            values: [wildcardType, ...result.value.types.keys()],
          },
          constructors: {
            tag: "Constructor",
            values: [wildcardType, ...result.value.constructors.keys()],
          },
          functions: {
            tag: "Function",
            values: [wildcardType, ...result.value.functions.keys()],
          },
          predicates: {
            tag: "Predicate",
            values: [wildcardType, ...result.value.predicates.keys()],
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

  displayNaturalLangOrPresetTabs = () => {
    const handleTabSwitch = (
      event: React.ChangeEvent<{}>,
      newValue: number,
    ) => {
      this.setState({ currentTab: newValue });
    };

    return (
      <>
        <Tabs
          textColor="primary"
          indicatorColor="primary"
          value={this.state.currentTab}
          onChange={handleTabSwitch}
        >
          <Tab label="Generate new problem" />
          <Tab label="Select from presets" />
        </Tabs>

        <br />

        <TabPanel index={0} currentTab={this.state.currentTab}>
          <TextField
            fullWidth
            multiline
            label="Description of input scenario"
            value={this.state.llmInput}
            onChange={(e) => {
              this.setState({ llmInput: e.target.value });
            }}
            variant="outlined"
          />
          <ButtonContainer>
            <Button
              onClick={this.onLLMGenerateClick}
              color="primary"
              variant="contained"
              disabled={this.state.llmRunning}
            >
              {this.state.llmRunning ? "Generating" : "Generate Input Scenario"}
            </Button>
          </ButtonContainer>
        </TabPanel>

        <TabPanel index={1} currentTab={this.state.currentTab}>
          <InputLabel id="preset-select-label">Preset</InputLabel>
          <Select
            key="preset"
            labelId="preset-select-label"
            id="preset-select"
            label="preset"
            // defaultValue={"c04p01"}
            value={this.state.presetSelect}
            onChange={(e) => {
              const key = e.target.value as string;
              this.handlePreset(key);

              const domainSelectStr = Object.entries(domains).find(
                ([_, { domain }]) => domain === presets[key].domain,
              )![0];

              this.setState({
                presetSelect: key,
                domainSelect: domainSelectStr,
              });
            }}
          >
            {this.presets()}
          </Select>
          <SettingLabel>Prompt:</SettingLabel>
          <div>
            <Latex>{this.state.prompt}</Latex>
          </div>
        </TabPanel>
      </>
    );
  };

  getSampleSubstancePreset = () => {
    return Object.entries(presets).find(
      ([_, { domain }]) => domain === this.state.domain,
    )![1];
  };

  getDomainPreset = () => {
    return Object.entries(domains).find(
      ([_, { domain }]) => domain === this.state.domain,
    )![1];
  };

  onLLMGenerateClick = () => {
    let output = "";

    this.setState({ llmRunning: true });

    const apiUrl = "https://api.openai.com/v1/chat/completions";

    const headers = {
      "Content-Type": "application/json",
      Authorization: `Bearer ${import.meta.env.VITE_OPENAI_API_KEY}`,
    };

    const samplePreset = this.getSampleSubstancePreset();

    const prompt = `
You are a code generator that is generating a new program in the Substance programming language, which draws from the Domain programming language program also given below. To write comments, begin with \`--\`. Return only the Substance program; explain your reasoning in Substance comments only.

We have been working on a platform called Penrose for authoring mathematical diagrams. The system involves a family of 3 domain specific languages: Substance (for specifying the mathematical objects and the relationships between those objects, Style (for mapping the mathematical objects to shapes and mathematical relationships to layout constraints and objectives), and Domain (for specifying the types of mathematical objects and relationships; this is a meta-language or schema language). Those three programs are used to synthesize a layout problem which we then solve to create a corresponding diagram.

Here is a Domain program which would inform a Substance program:

\`\`\`
${this.state.domain}
\`\`\`

Here is a sample Substance program named \"${samplePreset.displayName}\":

\`\`\`
${samplePreset.substance}
\`\`\`

Question: Given the context above, can you generate a new Substance program which describes the following: ${this.state.llmInput}?

To write comments, begin with \`--\`. Return only the Substance program; explain your reasoning in Substance comments only.`;

    console.log(prompt);
    const data = {
      model: "gpt-3.5-turbo",
      // model: "gpt-4",
      messages: [{ role: "user", content: prompt }],
      max_tokens: 2000,
      temperature: 0.1,
    };

    const start = Date.now();

    fetch(apiUrl, {
      method: "POST",
      headers: headers,
      body: JSON.stringify(data),
    })
      .then((response) => {
        console.log(Date.now() - start + "ms");
        return response.json();
      })
      .then((result) => {
        //console.log(result);
        // Process the result
        output = result.choices[0].message.content;

        // remove backticks from output
        output = output.replace(/`/g, "");

        this.setState({ substance: output, llmRunning: false });
      })
      .catch((error) => {
        // Handle any errors
        console.error("Error:", error);
        this.setState({ llmRunning: false });
      });
  };

  onGenerateClick = () => {
    if (this.state.setting)
      this.props.generateCallback(
        this.state.setting,
        this.state.seed,
        this.state.numPrograms,
        this.state.domain,
        this.state.substance,
        this.state.style,
        this.state.llmInput,
        this.state.currentTab,
        this.state.presetSelect,
        this.state.domainSelect,
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
  };

  onProgCountChange = (event: any, newValue: number | number[]) => {
    this.setState({ numPrograms: newValue as number });
  };

  onChangeMultiselect =
    (op: string, stmtType: string) => (selected: string[]) => {
      const typeSelect = (
        stmtType: string,
        op: DeclTypes,
        arr: string[],
      ): DeclTypes => {
        let newMatchSetting: MatchSetting = "*";
        if (!arr.includes(wildcardType)) {
          newMatchSetting = arr;
        }
        switch (stmtType) {
          case "Type":
            return { ...op, type: newMatchSetting };
          case "Constructor":
            return { ...op, constructor: newMatchSetting };
          case "Function":
            return { ...op, function: newMatchSetting };
          case "Predicate":
            return { ...op, predicate: newMatchSetting };
          default:
            return op;
        }
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
              defaults = v === "*" ? wildcardType : v;
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

  // NOTE: some graph domains are not yet included
  domains = () =>
    Object.entries(domains).map(([name, { displayName }]: [string, Preset]) => (
      <MenuItem key={name} value={name}>
        {displayName}
      </MenuItem>
    ));

  handlePreset = (key: string) => {
    this.setState({ ...this.state, ...presets[key] });
    this.updateDomainEnv(presets[key].domain);
    this.props.onPrompt(presets[key].prompt);
  };

  handleDomain = (key: string) => {
    this.setState({ ...this.state, ...domains[key] });
    this.updateDomainEnv(domains[key].domain);
  };

  componentDidMount = () => {
    // this.handlePreset("c04p01");
    this.handlePreset("lewis_0");
  };

  render() {
    return (
      <SettingsDrawer variant="permanent">
        {/* NOTE: Toolbar is used exclusively to space the content underneath the header of the page.
        The Settings element is floating so it must be included */}
        <Toolbar />
        <SettingContainer>
          <SettingDiv>
            <InputLabel id="domain-select-label">Pick a Domain</InputLabel>
            <Select
              key="domain"
              labelId="domain-select-label"
              id="domain-select"
              label="domain"
              value={this.state.domainSelect}
              onChange={(e) => {
                this.handleDomain(e.target.value as string);
                this.setState({
                  presetSelect: "",
                  domainSelect: e.target.value as string,
                });
              }}
            >
              {this.domains()}
            </Select>
          </SettingDiv>
          <SettingDiv>{this.displayNaturalLangOrPresetTabs()}</SettingDiv>
          <br />
          <Accordion key="substance" elevation={0}>
            <AccordionHeaderStyled>{`Input Scenario`}</AccordionHeaderStyled>
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
                height={"400px"}
                monacoOptions={{ theme: "vs" }}
                readOnly={false}
              />
            </AccordionBodyStyled>
          </Accordion>

          <SettingDiv>
            <SettingLabel>Mutator seed:</SettingLabel>
            <TextField
              id="standard-basic"
              // label="Mutator seed"
              variant="standard"
              value={this.state.seed}
              onChange={({ target }) => this.setState({ seed: target.value })}
            />
          </SettingDiv>
          <SettingDiv>
            <SettingLabel>Number of variations to generate:</SettingLabel>
            <Slider
              valueLabelDisplay="auto"
              step={1}
              marks={[
                { value: 1, label: "1" },
                { value: 10, label: "10" },
                { value: 20, label: "20" },
                { value: 30, label: "30" },
                { value: 40, label: "40" },
                { value: 50, label: "50" },
              ]}
              value={this.state.numPrograms}
              min={1}
              max={50}
              onChange={this.onProgCountChange}
            />
          </SettingDiv>
          {this.state.setting && (
            <WeightSlider
              divisions={[
                {
                  text: "Add",
                  percentage: this.state.setting!.opWeights.add * 100,
                  color: "#3f51b5",
                },
                {
                  text: "Delete",
                  percentage: this.state.setting!.opWeights.delete * 100,
                  color: "#3f51b5",
                },
                {
                  text: "Edit",
                  percentage: this.state.setting!.opWeights.edit * 100,
                  color: "#3f51b5",
                },
              ]}
              setDivisions={(divisions: any) => {
                if (this.state.setting) {
                  this.setState({
                    setting: {
                      ...this.state.setting,
                      opWeights: {
                        add: divisions[0].percentage / 100,
                        delete: divisions[1].percentage / 100,
                        edit: divisions[2].percentage / 100,
                      },
                    },
                  });
                }
              }}
            />
          )}
        </SettingContainer>
        <br />
        <ButtonContainer>
          <Button
            onClick={this.onGenerateClick}
            color="primary"
            variant="contained"
          >
            Generate Variations
          </Button>
        </ButtonContainer>
        <SettingDiv>
          <Accordion key="advanced" elevation={0}>
            <AccordionHeaderStyled>{`Advanced options`}</AccordionHeaderStyled>
            <AccordionBodyStyled style={{ padding: 0 }}>
              <SettingContainer>
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
                {this.inputElements()}
                <SettingLabel>Mutations per variation:</SettingLabel>
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
              </SettingContainer>
            </AccordionBodyStyled>
          </Accordion>
        </SettingDiv>
      </SettingsDrawer>
    );
  }
}
