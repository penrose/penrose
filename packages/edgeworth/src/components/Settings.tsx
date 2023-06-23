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
import { compileDomain, Env, showError } from "@penrose/core";
import c04p01 from "@penrose/examples/dist/geometry-domain/textbook_problems/c04p01.substance";
import React from "react";
import Latex from "react-latex-next";
import { OPENAI_API_KEY } from "../../env.js";
import { Preset, presets } from "../examples.js";
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

export interface SettingsProps {
  generateCallback: (
    setting: SynthesizerSetting,
    seed: string,
    numPrograms: number,
    dsl: string,
    prompt: string,
    sty: string,
    llmInput: string
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
      substance: c04p01,
      setting: undefined,
      numPrograms: 10,
      seed: "test0", // default seed
      domainEnv: defaultEnv,
      domain: this.props.defaultDomain,
      style: this.props.defaultStyle,
      prompt: "",
      llmInput: "",
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

  onGenerateClick = () => {
    if (this.state.setting)
      this.props.generateCallback(
        this.state.setting,
        this.state.seed,
        this.state.numPrograms,
        this.state.domain,
        this.state.substance,
        this.state.style,
        this.state.llmInput
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
        arr: string[]
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

  handlePreset = (key: string) => {
    this.setState({ ...this.state, ...presets[key] });
    this.updateDomainEnv(presets[key].domain);
    this.props.onPrompt(presets[key].prompt);
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
            <Select
              key="preset"
              labelId="preset-select-label"
              id="preset-select"
              label="preset"
              // defaultValue={"c04p01"}
              defaultValue={"lewis_0"}
              onChange={(e) => this.handlePreset(e.target.value as string)}
            >
              {this.presets()}
            </Select>
            <SettingLabel>Prompt:</SettingLabel>
            <div>
              <Latex>{this.state.prompt}</Latex>
            </div>
          </SettingDiv>
          <br />
          <TextField
            label="Enter text"
            value={this.state.llmInput}
            onChange={(e) => {
              this.setState({ llmInput: e.target.value });
            }}
            variant="outlined"
          />
          <ButtonContainer>
            <Button
              onClick={() => {
                let output = "";

                const apiUrl = "https://api.openai.com/v1/chat/completions";

                const headers = {
                  "Content-Type": "application/json",
                  Authorization: `Bearer ${OPENAI_API_KEY}`,
                };

                const prompt = `
                  You are a code generator that is generating a new program in the Substance programming language, 
                  which draws from the Domain programming language program also given below. To write comments, begin with \`--\`.\n
                  
                  We have been working on a platform called Penrose for authoring mathematical diagrams. 
                  The system involves a family of 3 domain specific languages: 
                  Substance (for specifying the mathematical objects and the relationships between those objects, 
                  Style (for mapping the mathematical objects to shapes and mathematical relationships to layout constraints and objectives), 
                  and Domain (for specifying the types of mathematical objects and relationships; this is a meta-language or schema language). 
                  Those three programs are used to synthesize a layout problem which we then solve to create a corresponding diagram.\n
                  
                  Here is a Domain program which would inform a Substance program:\n

                  \`\`\`\n
                  type Vertex\n
                  predicate Arc(Vertex a, Vertex b)\n
                  predicate HighlightVertex(Vertex a)\n
                  predicate HighlightArc(Vertex a, Vertex b)\n
                  \`\`\`\n

                  Here is a sample Substance program describing a directed graph:\n

                  \`\`\`\n
                  Vertex x, y, z, w, u, v\n
                  \n
                  Arc(x, y)\n
                  Arc(x, z)\n
                  Arc(y, z)\n
                  Arc(z, w)\n
                  Arc(w, u)\n
                  Arc(u, v)\n
                  Arc(x, v)\n
                  \n
                  HighlightVertex(x)\n
                  HighlightArc(x,y)\n
                  \n
                  AutoLabel All\n
                  \n
                  \`\`\`\n

                  According to Wikipedia, a Hamiltonian cycle (or Hamiltonian circuit) 
                  is a cycle that visits each vertex exactly once.\n

                  Question: Given the context above, can you generate a new Substance program which describes \n
                  ${this.state.llmInput}? Return only the Substance program; explain your reasoning in Substance comments.`;

                const data = {
                  model: "gpt-3.5-turbo-16k",
                  messages: [{ role: "user", content: `${prompt}` }],
                  max_tokens: 5000,
                  temperature: 0.7,
                };

                fetch(apiUrl, {
                  method: "POST",
                  headers: headers,
                  body: JSON.stringify(data),
                })
                  .then((response) => response.json())
                  .then((result) => {
                    // Process the result
                    console.log(result.choices[0]);
                    output = result.choices[0].message.content;
                    this.setState({ substance: output });
                  })
                  .catch((error) => {
                    // Handle any errors
                    console.error("Error:", error);
                  });
              }}
              color="primary"
              variant="contained"
            >
              Generate Input Scenario
            </Button>
          </ButtonContainer>
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
