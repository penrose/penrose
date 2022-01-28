import { TabNode } from "flexlayout-react";
import { FileDispatcher } from "../state/fileReducer";
import {
  FilePointer,
  FilePointerMap,
  IWorkspace,
  TrioType,
} from "../types/FileSystem";
import Select from "react-select";
import { useEffect, useState } from "react";

const mapFilterFiles = (openFiles: FilePointerMap, fileType: TrioType) =>
  Object.values(openFiles).filter(({ type }) => type === fileType);

type TrioSelection = {
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
    }
  }, [openFiles, trio]);
  useEffect(() => {
    setTrio((_trio: TrioSelection) => ({
      ..._trio,
      [fileType]: openFiles.length > 0 ? openFiles[0] : null,
    }));
  }, []);
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
}: {
  workspace: IWorkspace;
  node: TabNode;
}) {
  const [trio, setTrio] = useState<TrioSelection>({
    substance: null,
    style: null,
    domain: null,
  });
  return (
    <div>
      <h1>new diagram</h1>
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
  );
}
