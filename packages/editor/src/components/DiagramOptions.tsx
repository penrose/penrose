import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms";

export default function DiagramOptions() {
  const diagram = useRecoilValue(diagramState);

  return (
    <div>
      <label>
        variation: <input type="text" value={diagram.metadata.variation} />
      </label>
    </div>
  );
}
