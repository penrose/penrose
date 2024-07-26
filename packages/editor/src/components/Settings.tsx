import { allWarnings } from "@penrose/core";
import localforage from "localforage";
import { useEffect, useState } from "react";
import { ObjectInspector } from "react-inspector";
import Select from "react-select";
import { useRecoilState, useRecoilStateLoadable, useRecoilValue } from "recoil";
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
import StyleWrapper from "./StyleWrapper";

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
      localforage.setItem("balls", "xml tag lol");
    };
    set();
  }, [savedDiagrams]);

  if (settings.state !== "hasValue") {
    return <div>loading...</div>;
  }
  return (
    <StyleWrapper style={{ margin: "10px" }}>
      <h1>General Settings</h1>
      {currentUser != null ? (
        <div style={{ margin: "5px 0px" }}>
          <BlueButton onClick={useLogout}>Sign Out</BlueButton>
        </div>
      ) : (
        <div style={{ margin: "5px 0px" }}>
          <BlueButton onClick={useLogin}> Login with GitHub </BlueButton>
        </div>
      )}
      <div>
        <label>
          vim mode{" "}
          <input
            type="checkbox"
            checked={settings.contents.vimMode}
            onChange={(e) =>
              setSettings((state) => ({ ...state, vimMode: e.target.checked }))
            }
          />
        </label>
      </div>
      <p>exclude warnings: </p>
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
          control: (baseStyles, state) => ({
            ...baseStyles,
            fontSize: "12px",
            margin: "5px 0px 0p",
          }),
        }}
      />
      <div>
        <p>
          interactive mode (experimental) <br />
        </p>
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
        <label>Off</label>
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
        <label>Edit Mode</label>
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
        <label>Play Mode</label>
      </div>

      <h1>Variation Settings</h1>
      <div>
        <label>
          variation seed:
          <input
            style={{ margin: "5px 0px" }}
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
        </label>
        <div>
          <label>
            grid size:{" "}
            <input
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
            <output>{gridSize}</output>
          </label>
          <div>
            <BlueButton
              style={{ margin: "5px 0px" }}
              onClick={() => {
                setDiagramMetadata((metadata) => ({
                  ...metadata,
                  autostep: !metadata.autostep,
                }));
              }}
            >
              autostep ({diagramMetadata.autostep ? "on" : "off"})
            </BlueButton>
          </div>
        </div>
      </div>

      <h1>State</h1>
      <div>{state ? <ObjectInspector data={state} /> : <p>empty</p>}</div>

      {currentUser != null && (
        <div>
          <h1>Misc</h1>
          <p>
            We've migrated to cloud storage! You have {numLegacyDiagrams}{" "}
            unrestored locally stored diagrams.
          </p>
          {numLegacyDiagrams > 0 && (
            <BlueButton onClick={recoverAll}>Recover All</BlueButton>
          )}
        </div>
      )}
    </StyleWrapper>
  );
}
