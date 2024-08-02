import { Keycap } from "keycap";
import { Fragment, useEffect, useRef } from "react";
import { useRecoilState } from "recoil";
import styled, { ThemeProvider } from "styled-components";
import { showKeybindingsState } from "../state/atoms";
import { Plus } from "./Icons";

const CmdCtrl = () => (
  <>
    <Keycap activeKey="Meta">⌘</Keycap>
    <Keycap activeKey="Control">⌃</Keycap>
  </>
);

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
  keys: string[];
  name: string;
  description: string;
};

const generalKeys: HotKey[] = [
  {
    keys: ["CmdCtrl", "Enter"],
    name: "Compile",
    description: "Compile the current Penrose Trio",
  },
  {
    keys: ["CmdCtrl", "s"],
    name: "Save",
    description: "Save the current Penrose Trio",
  },
];

const editorKeys: HotKey[] = [
  {
    keys: ["CmdCtrl", "/"],
    name: "Toggle comment",
    description: "Toggle comment on the current line",
  },
  {
    keys: ["CmdCtrl", "Shift", "["],
    name: "Fold block",
    description: "Fold the current Style block",
  },
  {
    keys: ["CmdCtrl", "Shift", "]"],
    name: "Unfold block",
    description: "Unfold the current Style block",
  },
  {
    keys: ["CmdCtrl", "Alt", "["],
    name: "Fold all blocks",
    description: "Fold all Style blocks",
  },
  {
    keys: ["CmdCtrl", "Alt", "]"],
    name: "Unfold all blocks",
    description: "Unfold all Style blocks",
  },
];

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
  Meta: "⌘",
  Alt: "⌥",
};

const renderKey = (key: string) => {
  if (key === "CmdCtrl") {
    return <CmdCtrl />;
  } else
    return (
      <Keycap style={{ marginLeft: ".4em" }} activeKey={key}>
        {Object.keys(keyUnicode).includes(key) ? keyUnicode[key] : key}
      </Keycap>
    );
};

const KeyTable = ({ hotkeys }: { hotkeys: HotKey[] }) => (
  <TableContainer>
    <StyledTable>
      <TableBody>
        {hotkeys.map(({ keys, name, description }) => (
          <TableRow>
            <HeaderCellSpecial>
              <KeyBind>
                {keys.map(renderKey).map((key, index) => (
                  <Fragment key={index}>
                    {key}
                    {index < keys.length - 1 && <Plus />}
                  </Fragment>
                ))}
              </KeyBind>
            </HeaderCellSpecial>
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
        <h1>General</h1>
        <KeyTable hotkeys={generalKeys} />
        <h1>Program Editor</h1>
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
        Go to "settings" to enable Vim mode. Penrose's Vim mode is enabled by{" "}
        <TableLink href="https://codemirror.net/doc/manual.html#vim">
          codemirror-vim
        </TableLink>
        .
      </Container>
    </ThemeProvider>
  );
};
