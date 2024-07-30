import { allWarnings } from "@penrose/core";
import { useEffect, useState } from "react";
import { ObjectInspector } from "react-inspector";
import Select from "react-select";
import { useRecoilState, useRecoilStateLoadable, useRecoilValue } from "recoil";
import styled from "styled-components";
import {
  currentAppUser,
  diagramGridState,
  diagramMetadataSelector,
  diagramState,
  savedFilesState,
  settingsState,
} from "../state/atoms.js";
import {
  countLegacyDiagrams,
  useCompileDiagram,
  useRecoverAll,
} from "../state/callbacks.js";
import { logInWrapper, signOutWrapper } from "../utils/firebaseUtils.js";
import BlueButton from "./BlueButton.js";
import { SettingHeader, SettingLabel } from "./SettingElements.js";

const Section = styled.div`
  background-color: #f5f5f5;
  border-radius: 10px;
  padding: 10px;
`;

const Item = styled.div`
  color: #353538;
  font-size: 15px;
  display: flex;
  flex-direction: column;
  margin: 8px;
`;

const Checkbox = styled.input`
  margin-left: 5px;
  margin-bottom: 5px;
  vertical-align: middle;
`;

const TextInput = styled.input`
  border: 1px solid;
  margin: 5px 0px;
  color: #353538;
  border-color: hsl(0, 0%, 80%);
  border-radius: 4px;
  outline: none;
  font-size: 13px;
  padding: 5px;
	width: 100%

  &:focus {
    box-shadow: 0 0 0 1px #2584ff;
  }
`;

export default function Settings() {
  const [settings, setSettings] = useRecoilStateLoadable(settingsState);
  const [numLegacyDiagrams, setNumLegacyDiagrams] = useState<number>(0);
  const currentUser = useRecoilValue(currentAppUser);
  const { state } = useRecoilValue(diagramState);
  const useLogin = logInWrapper();
  const useLogout = signOutWrapper();
  const recoverAll = useRecoverAll();
  const savedDiagrams = useRecoilValue(savedFilesState);
  const [diagramMetadata, setDiagramMetadata] = useRecoilState(
    diagramMetadataSelector,
  );
  const [{ gridSize }, setSettingsState] = useRecoilState(diagramGridState);
  const compileDiagram = useCompileDiagram();

  useEffect(() => {
    const set = async () => {
      let num = await countLegacyDiagrams(savedDiagrams);
      setNumLegacyDiagrams(num);
    };
    set();
  }, [savedDiagrams]);

  if (settings.state !== "hasValue") {
    return <div>loading...</div>;
  }
  return (
    <div style={{ margin: "10px" }}>
      <SettingHeader>General</SettingHeader>
      <Section>
        {currentUser != null ? (
          <BlueButton onClick={useLogout}>Sign Out</BlueButton>
        ) : (
          <BlueButton onClick={useLogin}> Login with GitHub </BlueButton>
        )}
        <SettingLabel>
          <Item>
            <div>
              vim mode
              <Checkbox
                type="checkbox"
                checked={settings.contents.vimMode}
                onChange={(e) =>
                  setSettings((state) => ({
                    ...state,
                    vimMode: e.target.checked,
                  }))
                }
              />
            </div>
          </Item>
        </SettingLabel>
        <Item>exclude warnings: </Item>
        <Select
          options={allWarnings.map((tag) => ({ val: tag }))}
          isMulti
          isSearchable
          getOptionLabel={({ val }) => val}
          getOptionValue={({ val }) => val}
          value={diagramMetadata.excludeWarnings.map((tag) => ({
            val: tag,
          }))}
          onChange={(values) => {
            setDiagramMetadata((metadata) => ({
              ...metadata,
              excludeWarnings: values.map((v) => v.val),
            }));
          }}
          styles={{
            control: (baseStyles) => ({
              ...baseStyles,
              fontSize: "13px",
              minHeight: "30px",
            }),

            container: (baseStyles) => ({
              ...baseStyles,
              margin: "0 0 0 5px",
            }),

            option: (baseStyles) => ({
              ...baseStyles,
              fontSize: "13px",
            }),

            dropdownIndicator: (baseStyles) => ({
              ...baseStyles,
              padding: "3px",
            }),

            indicatorsContainer: (baseStyles) => ({
              ...baseStyles,
              padding: "3px",
            }),
          }}
        />
        <Item>
          interactive mode (experimental) <br />
        </Item>
        <input
          type="radio"
          name="interactivity"
          checked={settings.contents.interactive === "Off"}
          onChange={(e) => {
            setSettings((settings) => ({
              ...settings,
              interactive: "Off",
            }));
          }}
        />
        <SettingLabel>Off</SettingLabel>
        <br />
        <input
          type="radio"
          name="interactivity"
          checked={settings.contents.interactive === "EditMode"}
          onChange={(e) => {
            setSettings((settings) => ({
              ...settings,
              interactive: "EditMode",
            }));
          }}
        />
        <SettingLabel>Edit Mode</SettingLabel>
        <br />
        <input
          type="radio"
          name="interactivity"
          checked={settings.contents.interactive === "PlayMode"}
          onChange={(e) => {
            setSettings((settings) => ({
              ...settings,
              interactive: "PlayMode",
            }));
          }}
        />
        <SettingLabel>Play Mode</SettingLabel>
      </Section>
      <SettingHeader>Variations</SettingHeader>
      <Section>
        <Item>
          variation seed:
          <TextInput
            type="text"
            value={diagramMetadata.variation}
            onChange={(e) =>
              setDiagramMetadata((metadata) => ({
                ...metadata,
                variation: e.target.value,
              }))
            }
            onBlur={compileDiagram}
          />
        </Item>
        <Item>
          grid size:{" "}
          <input
            style={{ width: "80%", margin: "0 8px 0 0" }}
            type="range"
            min="1"
            max="30"
            value={gridSize}
            onChange={(e) =>
              setSettingsState((settings) => ({
                ...settings,
                gridSize: parseInt(e.target.value, 10),
              }))
            }
          />
          <output style={{ position: "relative", bottom: "3px" }}>
            {gridSize}
          </output>
        </Item>
        <BlueButton
          onClick={() => {
            setDiagramMetadata((metadata) => ({
              ...metadata,
              autostep: !metadata.autostep,
            }));
          }}
        >
          autostep ({diagramMetadata.autostep ? "on" : "off"})
        </BlueButton>
      </Section>

      {currentUser != null && (
        <div>
          <SettingHeader>Misc</SettingHeader>
          <Section>
            <Item>
              We've migrated to cloud storage! You have {numLegacyDiagrams}{" "}
              unrestored locally stored diagrams.
            </Item>
            {numLegacyDiagrams > 0 && (
              <BlueButton onClick={recoverAll}>Recover All</BlueButton>
            )}
          </Section>
        </div>
      )}

      <SettingHeader>State</SettingHeader>
      {state ? <ObjectInspector data={state} /> : <Item>empty</Item>}
    </div>
  );
}
