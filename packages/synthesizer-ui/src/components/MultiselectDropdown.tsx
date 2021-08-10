import {
  Chip,
  Input,
  InputLabel,
  MenuItem,
  Select,
  styled,
} from "@material-ui/core";
import React from "react";

export interface MultiselectDropdownProps {
  onChange: (selected: string[]) => void;
  mutationType: string;
  stmtType: string;
  defaults: string[];
}

interface State {
  selected: string[];
  options: string[];
}

const ChipDisplay = styled("div")({
  display: "flex",
  flexWrap: "wrap",
});

const StyledChip = styled(Chip)({
  margin: 2,
});

const ContainerDiv = styled("div")({
  paddingTop: "0.5rem",
});

export class MultiselectDropdown extends React.Component<
  MultiselectDropdownProps,
  State
> {
  constructor(props: MultiselectDropdownProps) {
    super(props);
    this.state = {
      selected: this.props.defaults,
      options: [],
    };
  }

  componentDidUpdate(prev: MultiselectDropdownProps) {
    if (
      prev.defaults !== this.props.defaults &&
      this.props.defaults.length > 0
    ) {
      const defaultStmts = new Set(this.props.defaults);
      this.setState({
        selected: [...this.props.defaults],
        options: this.state.options.filter((opt) => {
          return !defaultStmts.has(opt);
        }),
      });
    }
  }

  componentDidMount() {
    fetch("public/files/geometry.txt")
      .then((r) => r.text())
      .then((text) => {
        const lines = text.split("\n");
        const options = lines
          .filter((line) => line.startsWith(this.props.stmtType.toLowerCase()))
          .map((match) => {
            return match.split(" ")[1];
          });
        this.setState({ options }); // TODO subtract out default values
      });
  }

  onAdd = (event: any) => {
    console.log("changing", event.target.value);
    const newSelected = event.target.value as string[];
    const newOptions = this.state.options.filter((opt) => {
      const selected: Set<string> = new Set(event.target.value);
      return !selected.has(opt);
    });
    this.setState({
      selected: newSelected,
      options: newOptions,
    });
    this.props.onChange(newSelected);
  };

  onDelete = (chipToDelete: string) => {
    console.log("deleting a chip", chipToDelete);
    const newSelected = this.state.selected.filter(
      (chip) => chip !== chipToDelete
    );
    const newOptions = [...this.state.options, chipToDelete];
    this.setState({
      selected: newSelected,
      options: newOptions,
    });
    this.props.onChange(newSelected);
  };

  render() {
    return (
      <ContainerDiv>
        <InputLabel
          htmlFor={`${this.props.mutationType}-${this.props.stmtType}-input`}
        >{`${this.props.stmtType}s`}</InputLabel>
        <Select
          multiple
          value={this.state.selected}
          onChange={this.onAdd}
          fullWidth
          inputProps={{
            name: `${this.props.mutationType}-${this.props.stmtType}-input`,
            id: `${this.props.mutationType}-${this.props.stmtType}-input`,
          }}
          input={<Input />}
          renderValue={(selected) => (
            <ChipDisplay>
              {(selected as string[]).map((value) => (
                <StyledChip
                  variant="outlined"
                  color="primary"
                  key={value}
                  label={value}
                  size="small"
                  onDelete={() => this.onDelete(value)}
                  onMouseDown={(event: any) => {
                    // NOTE: necessary to intercept default behavior of Select: https://stackoverflow.com/a/60209711
                    event.stopPropagation();
                  }}
                />
              ))}
            </ChipDisplay>
          )}
        >
          {this.state.options.map((name) => (
            <MenuItem key={name} value={name}>
              {name}
            </MenuItem>
          ))}
        </Select>
      </ContainerDiv>
    );
  }
}
