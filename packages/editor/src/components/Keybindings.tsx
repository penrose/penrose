import { Fragment, useEffect, useRef } from "react";
import { useRecoilState } from "recoil";
import styled, { ThemeProvider } from "styled-components";
import { showKeybindingsState } from "../state/atoms";
import { Plus } from "./Icons";

const Keycap = styled.kbd`
  min-height: 0.8em;
  display: inline-flex;
  justify-content: center;
  align-items: center;
  padding: 0.15rem 0.275rem;
  background-color: white;
  border: 1px solid #e5e7eb; /* Equivalent to border-gray-200 */
  font-family: monospace;
  font-size: 0.875rem; /* Equivalent to text-sm */
  color: #1f2937; /* Equivalent to text-gray-800 */
  border-radius: 0.375rem; /* Equivalent to rounded-md */
  box-shadow: 0px 2px 0px 0px rgba(0, 0, 0, 0.08);
`;

const Container = styled.div`
  position: absolute;
  z-index: 1000;
  padding: 1rem 2rem;
  // centered in page
  // width: 30rem;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  background-color: #ffffff;
  border-radius: 10px;
  // shadow
  box-shadow: 0 0.25rem 0.5rem rgba(0, 0, 0, 0.1); /* Tailwind's shadow */
  color: ${({ theme }) => theme.textGray};
  font-size: 0.875rem;
`;

// Wrapper for the table container
const TableContainer = styled.div`
  position: relative;
  overflow-x: auto;
  // box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  border-radius: 0.5rem;
`;

// Table element with responsive and text styling
const StyledTable = styled.table`
  width: 100%;
  text-align: left;
  border-spacing: 0px;
`;

// Body section of the table
const TableBody = styled.tbody``;

// Row styling for table body
const TableRow = styled.tr`
  &:nth-child(odd) {
    background-color: ${({ theme }) => theme.bgWhite};
  }
  &:nth-child(even) {
    background-color: ${({ theme }) => theme.bgGrayLight};
  }
  border-bottom: 1px solid ${({ theme }) => theme.borderGray};
`;

const TableHeader = styled.thead`
  font-size: 0.75rem;
  color: #374151;
  text-transform: uppercase;
  background-color: #f9fafb;
  font-weight: 600;
`;

// Cell styling for table rows
const TableCell = styled.td`
  padding: 0.75rem 1.5rem;
  border: none;
`;

// Special styling for header cells
const HeaderCellSpecial = styled.th`
  padding: 0.75rem 1.5rem;
  font-weight: 500;
  // color: ${({ theme }) => theme.textGrayDark};
  white-space: nowrap;
`;

// Link styling inside table cells
const TableLink = styled.a`
  font-weight: 500;
  color: ${({ theme }) => theme.linkBlue};
  text-decoration: underline;
  &:hover {
    color: ${({ theme }) => theme.linkBlueHover};
  }
`;

const KeyBind = styled.span`
  display: flex;
  align-items: center;
`;

const theme = {
  textGray: "#4b5563",
  textGrayDark: "#1a202c",
  bgWhite: "#ffffff",
  bgGrayLight: "#f3f4f6",
  borderGray: "#e2e8f0",
  linkBlue: "#3490dc",
  linkBlueHover: "#2779bd",
};

type HotKey = {
  keys: {
    mac: string[];
    nonmac: string[];
  };
  name: string;
  description: string;
};

const generalKeys: HotKey[] = [];

const keyUnicode: { [key: string]: string } = {
  Enter: "↵",
  Tab: "⇥",
  Space: "␣",
  Shift: "⇧",
  Backspace: "⌫",
  ArrowUp: "↑",
  ArrowDown: "↓",
  ArrowLeft: "←",
  ArrowRight: "→",
  Escape: "⎋",
  Command: "⌘",
  Option: "⌥",
};

const editorKeys: HotKey[] = [
  {
    keys: {
      mac: [keyUnicode["Command"], "Enter"],
      nonmac: ["Ctrl", "Enter"],
    },
    name: "Compile",
    description: "Compile the current Penrose Trio",
  },
  {
    keys: {
      mac: [keyUnicode["Command"], "S"],
      nonmac: ["Ctrl", "S"],
    },
    name: "Save",
    description: "Save the current Penrose Trio",
  },
  {
    keys: {
      mac: [keyUnicode["Command"], "/"],
      nonmac: ["Ctrl", "/"],
    },
    name: "Toggle comment",
    description: "Toggle comment on the current line",
  },
  {
    keys: {
      mac: [keyUnicode["Command"], keyUnicode["Option"], "["],
      nonmac: ["Ctrl", "Alt", "["],
    },
    name: "Fold all blocks",
    description: "Fold all Style blocks",
  },
  {
    keys: {
      mac: [keyUnicode["Command"], keyUnicode["Option"], "]"],
      nonmac: ["Ctrl", "Alt", "]"],
    },
    name: "Unfold all blocks",
    description: "Unfold all Style blocks",
  },
];

const KeyTable = ({ hotkeys }: { hotkeys: HotKey[] }) => (
  <TableContainer>
    <StyledTable>
      <TableHeader>
        <HeaderCellSpecial>Mac</HeaderCellSpecial>
        <HeaderCellSpecial>Non-Mac</HeaderCellSpecial>
        <HeaderCellSpecial>Name</HeaderCellSpecial>
        <HeaderCellSpecial>Description</HeaderCellSpecial>
      </TableHeader>
      <TableBody>
        {hotkeys.map(({ keys, name, description }) => (
          <TableRow>
            <TableCell>
              <KeyBind>
                {keys.mac.map((key, index) => (
                  <Fragment key={index}>
                    <Keycap>{key}</Keycap>
                    {index < keys.mac.length - 1 && <Plus />}
                  </Fragment>
                ))}
              </KeyBind>
            </TableCell>
            <TableCell>
              <KeyBind>
                {keys.nonmac.map((key, index) => (
                  <Fragment key={index}>
                    <Keycap>{key}</Keycap>
                    {index < keys.nonmac.length - 1 && <Plus />}
                  </Fragment>
                ))}
              </KeyBind>
            </TableCell>
            <TableCell>{name}</TableCell>
            <TableCell>{description}</TableCell>
          </TableRow>
        ))}
      </TableBody>
    </StyledTable>
  </TableContainer>
);

export default () => {
  const overlayRef = useRef<HTMLDivElement>(null);
  const [showKeybindings, setShowKeybindings] =
    useRecoilState(showKeybindingsState);

  const handleClickOutside = (event: MouseEvent) => {
    if (
      overlayRef.current &&
      !overlayRef.current.contains(event.target as Node)
    ) {
      setShowKeybindings(false);
    }
  };

  useEffect(() => {
    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, []);
  return (
    <ThemeProvider theme={theme}>
      <Container ref={overlayRef}>
        {/* <h1>General</h1>
        <KeyTable hotkeys={generalKeys} /> */}
        <h1>Editor</h1>
        <p>
          Check out <code>defaultKeymap</code> and <code>standardKeymap</code>{" "}
          in the{" "}
          <TableLink href="https://codemirror.net/docs/ref/#commands">
            CodeMirror editor docs
          </TableLink>{" "}
          to see the full list of supported key bindings.
        </p>
        <KeyTable hotkeys={editorKeys} />
        <h1>Vim Mode</h1>
        Go to "settings" to enable Vim mode. Penrose's Vim mode is powered by{" "}
        <TableLink href="https://codemirror.net/doc/manual.html#vim">
          codemirror-vim
        </TableLink>
        .
      </Container>
    </ThemeProvider>
  );
};
