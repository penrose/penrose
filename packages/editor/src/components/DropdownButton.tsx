import { Menu, MenuItem } from "@material-ui/core";
import React from "react";
import BlueButton from "./BlueButton";

export interface DropdownItem {
  label: string;
  onClick: () => void;
}

export interface DropdownButtonProps {
  label: string;
  items: DropdownItem[];
}

export default function DropdownButton(props: DropdownButtonProps) {
  const [anchorEl, setAnchorEl] = React.useState<null | HTMLElement>(null);
  const isOpen = Boolean(anchorEl);
  const handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };
  const handleClose = () => {
    setAnchorEl(null);
  };

  const createMenuItems = () => {
    return props.items.map((item, i) => {
      const onClick = () => {
        item.onClick();
        handleClose();
      };
      // TODO casting bad?
      return (
        <MenuItem onClick={onClick} key={`download-dropdown-${i}`}>
          {item.label}
        </MenuItem>
      );
    });
  };

  return (
    <div>
      <BlueButton onClick={handleClick}>{props.label}</BlueButton>
      <Menu anchorEl={anchorEl} open={isOpen} onClose={handleClose}>
        {createMenuItems()}
      </Menu>
    </div>
  );
}
