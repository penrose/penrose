import { TabNode } from "flexlayout-react";
import { ReactElement, useState } from "react";
import styled from "styled-components";
import DiagramInitializer from "./DiagramInitializer";

const _Button = styled.button`
  border: none;
  border-radius: 10px;
  background-color: #f5f5f5;
  margin: 10px;
  width: 120px;
  height: 120px;
  display: flex;
  flex-direction: column;
  align-items: center;
  cursor: pointer;
  :hover {
    transition: background-color 0.2s;
    background-color: #f1f1f1;
  }
`;

const FileExtension = styled.div`
  font-size: 50px;
  font-family: serif;
  margin: 10px;
  color: #575757;
`;

const FileTypeButton = ({
  onClick,
  label,
  icon,
}: {
  onClick(): void;
  label: string;
  icon: ReactElement;
}) => {
  return (
    <_Button onClick={onClick}>
      {icon}
      <div>{label}</div>
    </_Button>
  );
};

export default function NewTab({ node }: { node: TabNode }) {
  const [currentView, setCurrentView] = useState<"all" | "diagram">("all");
  if (currentView === "diagram") {
    return <DiagramInitializer node={node} />;
  }
  return (
    <div>
      <h1>create</h1>
      <div style={{ display: "flex", flexDirection: "row", flexWrap: "wrap" }}>
        <FileTypeButton
          label="diagram"
          icon={<FileExtension>.svg</FileExtension>}
          onClick={() => setCurrentView("diagram")}
        />
        <FileTypeButton
          label="substance"
          icon={<FileExtension>.sub</FileExtension>}
          onClick={() => setCurrentView("diagram")}
        />
        <FileTypeButton
          label="trio"
          icon={<FileExtension>,,,</FileExtension>}
          onClick={() => setCurrentView("diagram")}
        />
      </div>
    </div>
  );
}
