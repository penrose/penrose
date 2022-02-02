import { TabNode } from "flexlayout-react";
import {
  FilePointer,
  FilePointerMap,
  IWorkspace,
  TrioType,
} from "../types/FileSystem";
import Select from "react-select";
import { useEffect, useState } from "react";
import { BigBlueButton } from "./BlueButton";

const mapFilterFiles = (openFiles: FilePointerMap, fileType: TrioType) =>
  Object.values(openFiles).filter(({ type }) => type === fileType);

export type TrioSelection = {
  substance: FilePointer | null;
  style: FilePointer | null;
  domain: FilePointer | null;
};

const FileSelector = ({
  workspace,
  fileType,
  trio,
  setTrio,
}: {
  workspace: IWorkspace;
  trio: TrioSelection;
  setTrio(trio: (trio: TrioSelection) => TrioSelection): void;
  fileType: TrioType;
}) => {
  const openFiles = mapFilterFiles(workspace.openFiles, fileType);
  //   Make sure selected file is still open
  useEffect(() => {
    if (
      trio[fileType] !== null &&
      !(trio[fileType]!.id in workspace.openFiles)
    ) {
      setTrio((_trio) => ({ ..._trio, [fileType]: null }));
    } else if (trio[fileType] === null && openFiles.length > 0) {
      setTrio((_trio: TrioSelection) => ({
        ..._trio,
        [fileType]: openFiles[0],
      }));
    }
  }, [openFiles, trio]);
  if (openFiles.length === 0) {
    return (
      <div style={{ color: "#666666" }}>no {fileType} open to choose from</div>
    );
  }
  return (
    <Select
      aria-label={`Select ${fileType} file`}
      options={openFiles}
      getOptionLabel={({ name }: FilePointer) => name}
      getOptionValue={({ id }: FilePointer) => id}
      name={fileType}
      value={trio[fileType]}
      onChange={(selected: FilePointer | null) =>
        setTrio((_trio: TrioSelection) => ({
          ..._trio,
          [fileType]: selected,
        }))
      }
      noOptionsMessage={() => `No ${fileType}s`}
      isSearchable
    />
  );
};

export default function DiagramInitializer({
  workspace,
  node,
  updateNodeToNewDiagram,
}: {
  workspace: IWorkspace;
  node: TabNode;
  updateNodeToNewDiagram(
    node: TabNode,
    trioSelection: TrioSelection,
    autostep: boolean
  ): void;
}) {
  const [trio, setTrio] = useState<TrioSelection>({
    substance: null,
    style: null,
    domain: null,
  });
  const [autostep, setAutostep] = useState(true);
  return (
    <div>
      <h1>new diagram</h1>
      <div>
        <span>domain</span>
        <FileSelector
          workspace={workspace}
          trio={trio}
          setTrio={setTrio}
          fileType="domain"
        />
        <span>style</span>
        <FileSelector
          workspace={workspace}
          trio={trio}
          setTrio={setTrio}
          fileType="style"
        />
        <span>substance</span>
        <FileSelector
          workspace={workspace}
          trio={trio}
          setTrio={setTrio}
          fileType="substance"
        />
      </div>
      <div>
        <label>
          <input
            type="checkbox"
            checked={autostep}
            onChange={(e) => setAutostep(e.target.checked)}
          />
          autostep
        </label>
      </div>
      <BigBlueButton
        onClick={() => updateNodeToNewDiagram(node, trio, autostep)}
        disabled={Object.values(trio).includes(null)}
      >
        create
      </BigBlueButton>
    </div>
  );
}
