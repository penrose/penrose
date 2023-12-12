import { Menu, MenuItem } from "@material-ui/core";
import React, { ReactNode } from "react";
import BlueButton from "./BlueButton";

export interface DropdownItem {
  label: string;
  onClick: () => void;
}

export interface DropdownButtonProps {
  label: string;
  items: DropdownItem[];
}

export interface DropdownButtonState {
  isOpen: boolean;
}

export class DropdownButton extends React.Component<
  DropdownButtonProps,
  DropdownButtonState
> {
  anchorEl: HTMLElement | null;
  setAnchorEl: React.Dispatch<React.SetStateAction<HTMLElement | null>>;
  isOpen: boolean;
  constructor(props: DropdownButtonProps) {
    super(props);
    [this.anchorEl, this.setAnchorEl] = React.useState<null | HTMLElement>(
      null,
    );
    this.isOpen = Boolean(this.anchorEl);
  }

  handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    this.setAnchorEl(event.currentTarget);
  };

  handleClose = () => {
    this.setAnchorEl(null);
  };

  createMenuItems = () => {
    return this.props.items.map((i) => {
      const onClick = () => {
        i.onClick();
        this.handleClose();
      };
      // TODO casting bad?
      return (<MenuItem onClick={onClick}>{i.label}</MenuItem>) as ReactNode;
    });
  };

  render() {
    return (
      <div>
        <BlueButton onClick={this.handleClick}>{this.props.label}</BlueButton>
        <Menu
          anchorEl={this.anchorEl}
          open={this.isOpen}
          onClose={this.handleClose}
        >
          {this.createMenuItems()}
        </Menu>
      </div>
    );
  }
}
