import React from "react";
import { SynthesizerSetting } from "../../../core/build/dist";
import { Grid } from "./Grid";
import { Header } from "./Header";

export interface ContentProps {
  content: any[];
}

const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 4],
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
    constructor: [],
    predicate: ["Equal"],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
  },
  edit: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
  },
};

export class Content extends React.Component {
  constructor(props: ContentProps) {
    super(props);
    this.setState({
      setting: defaultSetting,
      progs: [],
      style: "",
      domain: "",
    });
  }

  updateSettings = (newSetting: SynthesizerSetting) => {
    this.setState({
      ...this.state,
      setting: newSetting,
    });
  };

  render() {
    return (
      <div>
        <Header />
        <Grid />
      </div>
    );
  }
}
