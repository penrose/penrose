import * as React from "react";
import styled from "styled-components";
import Select, { components } from "react-select";

const D = styled.div`
  margin: 1em;
  display: inline-block;
  width: 200px;
`;

const MiddleAlign = styled.div`
  display: flex;
  align-items: center;
  span {
    margin-left: 0.1em;
    margin-right: 0.2em;
    font-size: 1em;
  }
  img {
    margin-left: 0.2em;
    margin-right: 0.1em;
  }
`;
const Option = (props: any) => {
  return (
    <components.Option {...props}>
      <MiddleAlign>
        <img src={props.data.icon} width={20} /> <span>{props.data.label}</span>
      </MiddleAlign>
    </components.Option>
  );
};

const ValueContainer = (props: any) => {
  const val = props.getValue()[0];
  return (
    <components.ValueContainer {...props}>
      <MiddleAlign style={{ fontWeight: "bold" }}>
        {val && <img src={val.icon} width={20} />} <span>{props.children}</span>
      </MiddleAlign>
    </components.ValueContainer>
  );
};

export interface IOption {
  value: number;
  label: string;
  icon: string;
}
interface IProps {
  options: IOption[];
  selected: IOption;
  onSelect: (option: IOption) => void;
}

interface IState {
  openTray: boolean;
  hovering: number;
  intermediateSelection: number;
}
class DropdownMenu extends React.Component<IProps, IState> {
  public onChange = (value: IOption) => {
    this.props.onSelect(value);
  };

  public render() {
    const { selected, options } = this.props;
    return (
      <D>
        <Select
          aria-label="Element Program"
          isClearable={false}
          backspaceRemovesValue={false}
          isSearchable={true}
          styles={{
            option: (base: any, { isSelected }: any) => ({
              ...base,
              color: isSelected ? "#40B4F7" : "#393939",
              fontWeight: isSelected ? "bold" : 400
            }),
            indicatorSeparator: () => ({
              display: "none"
            }),
            control: (base: any) => ({
              ...base,
              border: "0.2ex solid #40b4f7 !important"
            })
          }}
          theme={(theme: any) => ({
            ...theme,
            borderRadius: "6px",
            spacing: { ...theme.spacing, menuGutter: "0px" },
            colors: {
              ...theme.colors,
              selected: "#40b47",
              primary25: "#edf8ff",
              primary: "#e5f4ff",
              neutral80: "#40b4f7"
            }
          })}
          components={{ Option, ValueContainer }}
          options={options}
          value={selected}
          onChange={this.onChange}
        />
      </D>
    );
  }
}

export default DropdownMenu;
