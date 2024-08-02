import { useCallback, useState } from "react";
import { useMediaQuery } from "react-responsive";
import { useRecoilCallback, useRecoilState, useRecoilValue } from "recoil";
import styled from "styled-components";
import {
  currentWorkspaceState,
  diagramWorkerState,
  settingsState,
  showKeybindingsState,
} from "../state/atoms.js";
import {
  autosaveHook,
  saveShortcutHook,
  useCompileDiagram,
  useIsUnsaved,
  useNewWorkspace,
  usePublishGist,
  useResampleDiagram,
  useSaveNewWorkspace,
  useSaveWorkspace,
} from "../state/callbacks.js";
import BlueButton from "./BlueButton.js";
import Dropdown from "./Dropdown.js";
import { Discord, Docs, Keyboard, Tutorial } from "./Icons.js";
import Keybindings from "./Keybindings.js";
const UnsavedIcon = styled.div`
  background-color: #dddddd;
  padding: 2px 4px;
  border-radius: 2px;
  font-size: 10px;
  color: #111111;
`;

const TitleBox = styled.div`
  padding: 5px 10px;
  cursor: text;
  box-sizing: border-box;
  border: 1px solid rgba(0, 0, 0, 0);
  font-size: 15px;
  :hover {
    border: 1px solid gray;
    border-radius: 5px;
  }
`;

const AuthorBox = styled.div`
  border-radius: 5px;
  transition: 0.2s;
  display: flex;
  flex-direction: row;
  align-items: center;
  margin: 2px;
  padding: 2px 5px;
  :hover {
    transition: 0.2s;
    background-color: rgba(0, 0, 0, 0.1);
  }
`;

const InputBox = styled.input`
  padding: 5px 10px;
  cursor: text;
  box-sizing: border-box;
  border: 1px solid rgba(0, 0, 0, 0);
  font-size: 15px;
`;

const HeaderButtonContainer = styled.div`
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 0.3em;
`;

const ButtonsAlignment = styled.div<{ isMobile: boolean }>`
  margin: ${(props) => (props.isMobile ? "0 0 0 auto" : "0")};
`;

function EditableTitle() {
  const [editing, setEditing] = useState(false);
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const saveWorkspace = useSaveWorkspace();
  const isMobile = useMediaQuery({ query: "(max-width: 768px)" });
  const maxMobileTitleLen = 20;

  const onChange = useRecoilCallback(
    ({ set }) =>
      (e: React.ChangeEvent<HTMLInputElement>) => {
        set(currentWorkspaceState, (state) => ({
          ...state,
          metadata: {
            ...state.metadata,
            name: e.target.value,
          },
        }));
      },
  );

  const onFinish = () => {
    if (currentWorkspace.metadata.location.kind == "stored") {
      saveWorkspace();
    }
    setEditing(false);
  };

  const onKey = useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key === "Enter" || e.key === "Escape") {
        if (currentWorkspace.metadata.location.kind == "stored") {
          saveWorkspace();
        }
        setEditing(false);
      }
    },
    [currentWorkspace],
  );
  if (editing) {
    return (
      <InputBox
        type="text"
        value={currentWorkspace.metadata.name}
        autoFocus={true}
        onFocus={(e) => e.target.select()}
        onBlur={onFinish}
        onKeyDown={onKey}
        onChange={onChange}
      />
    );
  }
  return (
    <TitleBox onClick={() => setEditing(true)}>
      {isMobile && currentWorkspace.metadata.name.length > maxMobileTitleLen
        ? currentWorkspace.metadata.name.slice(0, maxMobileTitleLen) + "..."
        : currentWorkspace.metadata.name}
    </TitleBox>
  );
}

export default function TopBar() {
  const compileDiagram = useCompileDiagram();
  const resampleDiagram = useResampleDiagram();
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const settings = useRecoilValue(settingsState);
  const publishGist = usePublishGist();
  const { optimizing, compiling } = useRecoilValue(diagramWorkerState);
  const isUnsaved = useIsUnsaved();
  const newWorkspace = useNewWorkspace();
  const saveWorkspace = useSaveWorkspace();
  const saveNewWorkspace = useSaveNewWorkspace();
  const [showKeybindings, setShowKeybindings] =
    useRecoilState(showKeybindingsState);
  const isMobile = useMediaQuery({ query: "(max-width: 768px)" });

  /**
   * These hooks are here because 1) In App, the call to get value of
   * currentWorkspace in autosaveHook creates a noticable slowdown. 2) This
   * component exists on all loads (why we put here and not SavedBrowser)
   */
  saveShortcutHook();
  autosaveHook();

  return (
    <nav
      style={{
        display: "flex",
        width: "100%",
        backgroundColor: "#F4F4F4",
        justifyContent: "space-between",
        alignItems: "center",
        padding: "10px",
        boxSizing: "border-box",
      }}
    >
      {currentWorkspace.metadata.location.kind === "roger" ? (
        <div>loaded from filesystem via Roger</div>
      ) : (
        <div
          style={{
            display: "flex",
            flexDirection: "row",
            alignItems: "center",
            width: "100%",
          }}
        >
          <EditableTitle />
          {isUnsaved() ? <UnsavedIcon>unsaved</UnsavedIcon> : ""}
          {currentWorkspace.metadata.location.kind === "gist" && (
            <a
              style={{ textDecoration: "none", color: "inherit" }}
              href={`https://github.com/${currentWorkspace.metadata.location.author}`}
            >
              <AuthorBox>
                <div
                  style={{
                    width: "25px",
                    height: "25px",
                    margin: "5px",
                    backgroundImage: `url(${currentWorkspace.metadata.location.avatar})`,
                    borderRadius: "50%",
                    backgroundSize: "cover",
                    display: "inline-block",
                  }}
                />{" "}
                {currentWorkspace.metadata.location.author}
              </AuthorBox>
            </a>
          )}
          <ButtonsAlignment isMobile={isMobile}>
            {currentWorkspace.metadata.location.kind == "local" &&
              currentWorkspace.metadata.location.changesMade && (
                <BlueButton
                  // isMobile={isMobile}
                  onClick={() =>
                    saveNewWorkspace(
                      currentWorkspace.metadata.id,
                      currentWorkspace,
                    )
                  }
                >
                  save
                </BlueButton>
              )}
            {currentWorkspace.metadata.location.kind === "stored" &&
              !currentWorkspace.metadata.location.saved && (
                <BlueButton onClick={() => saveWorkspace()}>save</BlueButton>
              )}
            {currentWorkspace.metadata.location.kind === "stored" &&
              currentWorkspace.metadata.location.saved &&
              settings.githubAccessToken !== null && (
                <BlueButton onClick={publishGist}>share</BlueButton>
              )}

            <BlueButton onClick={newWorkspace}>
              new {!isMobile && "workspace"}
            </BlueButton>
          </ButtonsAlignment>
        </div>
      )}
      {!isMobile && (
        <HeaderButtonContainer>
          <Dropdown
            items={[
              {
                text: "Do the tutorial",
                link: "https://penrose.cs.cmu.edu/docs/tutorial/welcome",
                icon: <Tutorial />,
              },
              {
                text: "Read the docs",
                link: "https://penrose.cs.cmu.edu/docs/ref",
                icon: <Docs />,
              },
              {
                text: "Community help",
                link: "https://discord.gg/a7VXJU4dfR",
                icon: <Discord />,
              },
              {
                text: "View hotkeys",
                icon: <Keyboard />,
                onClick: () => setShowKeybindings(true),
              },
            ]}
          />
          <BlueButton disabled={compiling} onClick={resampleDiagram}>
            resample
          </BlueButton>
          <BlueButton disabled={compiling} onClick={compileDiagram}>
            compile
          </BlueButton>
        </HeaderButtonContainer>
      )}
      {showKeybindings && <Keybindings />}
    </nav>
  );
}
