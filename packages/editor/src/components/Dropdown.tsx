import { useEffect, useRef, useState } from "react";
import styled from "styled-components";

const Help = styled.button`
  background-color: #c9c9c9;
  border: solid 0.5px;
  color: #ffffff;
  border-radius: 100%;
  font-weight: bold;
  width: 1.5rem;
  height: 1.5rem;
  display: flex;
  justify-content: center;
  align-items: center;
  font-size: 1rem;
  cursor: pointer;
  :hover {
    background-color: #d9d9d9;
    transition: 0.2s;
  }
`;

export const DropdownMenu = styled.div`
  z-index: 10;
  background-color: white;
  margin-top: 0.5rem;
  border-radius: 0.5rem; /* Tailwind's rounded-lg */
  box-shadow: 0 0.25rem 0.5rem rgba(0, 0, 0, 0.1); /* Tailwind's shadow */
  width: 10.5rem;
  position: absolute; /* Adjust position based on needs */
`;

// Dropdown menu items list
export const DropdownList = styled.ul`
  margin-block-start: 0.4rem;
  margin-block-end: 0.4rem;
  padding: 0 0.05rem;
  font-size: 0.875rem; /* Tailwind's text-sm */
  list-style-type: none;
  color: #4b5563; /* Tailwind's text-gray-700 */
`;

// Dropdown menu item
export const DropdownItem = styled.li`
  cursor: pointer;
  &:hover {
    background-color: #f3f4f6; /* Tailwind's hover:bg-gray-100 */
  }
`;

// Link styled-component within dropdown
export const DropdownLink = styled.a`
  display: flex;
  padding: 0.5rem 1rem; /* Tailwind's px-4 py-2 */
  text-decoration: none;
  gap: 0.5rem;
  color: inherit;
`;

export type DropDownItem = {
  text: string;
  link?: string;
  icon: JSX.Element;
  onClick?: () => void;
};

export default ({ items }: { items: DropDownItem[] }) => {
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement>(null);

  const handleClickOutside = (event: MouseEvent) => {
    if (
      dropdownRef.current &&
      !dropdownRef.current.contains(event.target as Node)
    ) {
      setIsOpen(false);
    }
  };

  useEffect(() => {
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, []);
  return (
    <div>
      <Help onClick={() => setIsOpen(!isOpen)}>?</Help>
      {isOpen && (
        <DropdownMenu ref={dropdownRef}>
          <DropdownList aria-labelledby="dropdownDefaultButton">
            {items.map((item) => (
              <DropdownItem
                key={item.text}
                onClick={(e) => {
                  setIsOpen(false);
                  if (item.onClick) item.onClick();
                }}
              >
                <DropdownLink href={item.link} target="_blank">
                  {item.icon}
                  {item.text}
                </DropdownLink>
              </DropdownItem>
            ))}
          </DropdownList>
        </DropdownMenu>
      )}
    </div>
  );
};
