import { FileUploader } from "react-drag-drop-files";
import { useRecoilState } from "recoil";
import { ProgramType, currentProgramSelector } from "../state/atoms";
const ProgramUploader = ({ programType }: { programType: ProgramType }) => {
  const [programContent, setProgramContent] = useRecoilState(
    currentProgramSelector(programType),
  );

  return (
    <FileUploader handleFile={setProgramContent}>
      <div style={{ border: "2px dashed" }}>
        <p>Click here to upload a {programType} program</p>
      </div>
    </FileUploader>
  );
};

export default ProgramUploader;
