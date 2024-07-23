import Chip from "@material-ui/core/Chip";

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
  Toolbar,
  Typography,
  styled,
} from "@material-ui/core";
import { Listing } from "@penrose/components";
import { compileDomain, showError } from "@penrose/core";
import { DomainEnv } from "@penrose/core/dist/types/domain";
import euclideanStyleMin from "@penrose/examples/dist/geometry-domain/euclidean.min.style.js";
import geometryDomainMin from "@penrose/examples/dist/geometry-domain/geometry.min.domain";
import React from "react";
import { Preset, domains } from "../examples.js";
import { SynthesizerSetting } from "../synthesis/Synthesizer.js";
import { wildcardType } from "../util.js";
import { Refresh } from "./Grid.js";

import animalNameList from "animals";
import colorNameList from "color-name-list/dist/colornames.json";

// all one-word colors
const colors: string[] = colorNameList
  .map(({ name }) => name)
  .filter((color) => /^[A-Z][a-z]+$/.test(color));

// all one-word animals, with first letter capitalized
const animals: string[] = animalNameList.words
  .filter((animal: string) => /^[a-z]+$/.test(animal))
  .map((animal: string) => animal.charAt(0).toUpperCase() + animal.slice(1));

// min and max are both inclusive
const randInt = (min: number, max: number) =>
  Math.floor(Math.random() * (max + 1 - min)) + min;

const choose = (list: string[]) =>
  list[Math.floor(Math.random() * list.length)];

export const generateVariation = (): string => {
  return `${choose(colors)}${choose(animals)}`;
};

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

export interface SettingsProps {
  generateCallback: (
    setting: SynthesizerSetting,
    seed: string,
    numPrograms: number,
    dsl: string,
    sty: string,
    domainSelect: string,
  ) => Promise<void>;
}

interface SettingState {
  substance: string;
  setting: SynthesizerSetting | undefined;
  seed: string;
  numPrograms: number;
  domainEnv: PartialEnv;
  domain: string;
  style: string;
  domainSelect: string;
  env?: DomainEnv;
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
  gap: "1rem",
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

    const key = "Geometry";
    const preset = domains[key][0];
    const domain = preset.domain;
    const style = preset.style;

    const result = compileDomain(domain);
    let env;
    if (result.isOk()) {
      env = {
        types: {
          tag: "Type",
          values: [wildcardType, ...result.value.types.keys()],
        },
        constructors: {
          tag: "Constructor",
          values: [wildcardType, ...result.value.constructorDecls.keys()],
        },
        functions: {
          tag: "Function",
          values: [wildcardType, ...result.value.functionDecls.keys()],
        },
        predicates: {
          tag: "Predicate",
          values: [wildcardType, ...result.value.predicateDecls.keys()],
        },
      };
    } else {
      // display error message
      console.error(showError(result.error));
    }

    this.state = {
      substance: "",
      numPrograms: 9,
      seed: "test0", // default seed
      domainEnv: env!,
      domainSelect: key,
      style: style,
      setting: preset.setting,
      domain: domain,
    };
  }

  // TODO: current implementation will not update the UI if there is a domain compiler error
  // at any point in time, instead, we should display a helpful error message.
  updateDomainEnv = (newDomain: string) => {
    const result = compileDomain(newDomain);
    if (result.isOk()) {
      console.log("compiling domain");
      this.setState({
        domainEnv: {
          types: {
            tag: "Type",
            values: [wildcardType, ...result.value.types.keys()],
          },
          constructors: {
            tag: "Constructor",
            values: [wildcardType, ...result.value.constructorDecls.keys()],
          },
          functions: {
            tag: "Function",
            values: [wildcardType, ...result.value.functionDecls.keys()],
          },
          predicates: {
            tag: "Predicate",
            values: [wildcardType, ...result.value.predicateDecls.keys()],
          },
        },
      });
    } else {
      // display error message
      console.error(showError(result.error));
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
      );
    else {
      console.log("no settings in the state");
    }
  };

  // NOTE: some graph domains are not yet included
  domains = () =>
    Object.entries(domains).map(([name]: [string, Preset[]]) => (
      <MenuItem key={name} value={name}>
        {name}
      </MenuItem>
    ));

  handleDomain = (key: string) => {
    // NOTE: all programs in `domains` have the same domain program, therefore picking the first program to access the domain program
    const preset = domains[key][0];
    const domain = key === "Geometry" ? geometryDomainMin : preset.domain;
    const style = key === "Geometry" ? euclideanStyleMin : preset.style;
    this.updateDomainEnv(domain);
    this.setState({
      ...this.state,
      domainSelect: key,
      style: style,
      substance: this.state.substance, // changing domains doesn't change the substance by default
      setting: preset.setting,
      domain: domain,
    });
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
                  domainSelect: e.target.value as string,
                });
              }}
            >
              {this.domains()}
            </Select>
          </SettingDiv>
          <Accordion key="substance" elevation={0} defaultExpanded>
            <AccordionHeaderStyled>{`Input Scenario`}</AccordionHeaderStyled>
            <AccordionBodyStyled style={{ padding: 0 }}>
              <Listing
                language="substance"
                domain={this.state.domain}
                src={this.state.substance}
                onChange={(sub: string) =>
                  this.setState({
                    substance: sub,
                  })
                }
                width={"100%"}
                height={"400px"}
                readOnly={false}
                darkMode={false}
              />
            </AccordionBodyStyled>
          </Accordion>
        </SettingContainer>
        <ButtonContainer>
          <Chip label={`variation seed: ${this.state.seed}`} disabled />
        </ButtonContainer>
        <ButtonContainer>
          <Button
            onClick={this.onGenerateClick}
            color="primary"
            variant="contained"
          >
            Generate Variations
          </Button>
          <Button
            onClick={() => {
              this.setState({ seed: generateVariation() }, () => {
                this.onGenerateClick();
              });
            }}
            color="primary"
            variant="contained"
          >
            <Refresh></Refresh>
            More Variations
          </Button>
        </ButtonContainer>
      </SettingsDrawer>
    );
  }
}
