import { ObjectInspector } from "react-inspector";
import { useRecoilValue } from "recoil";
import { diagramState } from "../state/atoms.js";

// https://goessner.net/articles/JsonPath/
export default function StateInspector() {
  const { state } = useRecoilValue(diagramState);
  if (state === null) {
    return <p style={{ padding: "1em" }}>empty</p>;
  }
  return (
    <div style={{ padding: "1em" }}>
      {state ? <ObjectInspector data={state} /> : <p>empty</p>}
    </div>
  );
}
