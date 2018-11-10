import * as React from "react";
import styled from "styled-components";
import Select, { components } from "react-select";
// import Button from "Button";
// import ChevronDownIcon from "./chevron_down.svg";
// import ChevronUpIcon from "./chevron_up.svg";

const D = styled.div`
  display: inline-block;
  width: 200px;
`;

// const B = styled.div`
//   display: flex;
//   align-items: center;
// `;

// const Container = styled.div`
//   margin: 0 0.3em 0 0.3em;
//   position: absolute;
//   display: inline-block;
//   z-index: 1000;
//   background-color: #ffffff;
//   border: 0.2ex solid #40b4f7;
//   border-radius: 6px;
//   overflow: hidden;
// `;

// const Item = styled.div`
//   display: block;
//   padding: 0.15em 0.3em 0.15em 0.3em;
//   display: flex;
//   align-items: center;
//   justify-content: space-between;
//   cursor: pointer;
//   user-select: none;
//   color: #40b4f7;
//   background-color: ${({ selected }: any) =>
//     selected ? "#e5f4ff" : "default"};
//   font-weight: ${({ selected }: any) => (selected ? "bold" : "600")};
//   span {
//     margin-left: 0.1em;
//     margin-right: 0.2em;
//   }
//   img {
//     margin-left: 0.2em;
//     margin-right: 0.1em;
//   }
//   /* transition: 0.2s; */
// // `;

// const Icon = styled.img`
//   cursor: pointer;
// `;

// interface IFullItemProps {
//   label: string;
//   icon: string;
//   index: number;
//   selected: boolean;
//   onClick(index: number): void;
//   onHover(index: number): void;
// }

// const FullItem = ({
//   label,
//   index,
//   icon,
//   onClick,
//   onHover,
//   selected
// }: IFullItemProps) => (
//   <Item
//     tabIndex={index}
//     onClick={() => onClick(index)}
//     onMouseEnter={() => onHover(index)}
//     selected={selected}
//   >
//     <span>{label}</span>
//     <img src={icon} width={20} />
//   </Item>
// );
const MiddleAlign = styled.div`
  display: flex;
  align-items: center;
  span {
    margin-left: 0.1em;
    margin-right: 0.2em;
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
      <MiddleAlign>
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
            control: (base: any) => ({ ...base, border: "0.2ex solid #40b4f7" })
          }}
          theme={(theme: any) => ({
            ...theme,
            borderRadius: "6px",
            spacing: { ...theme.spacing, menuGutter: "0px" },
            colors: {
              ...theme.colors,
              selected: "#40b47",
              primary25: "#edf8ff",
              primary: "#e5f4ff"
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
