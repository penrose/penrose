import { useRecoilValueLoadable } from "recoil";
import { exampleTriosState } from "../state/atoms";
import { useLoadExampleWorkspace } from "../state/callbacks";
import FileButton from "./FileButton";

export default function ExamplesBrowser() {
  const examples = useRecoilValueLoadable(exampleTriosState);
  const loadExample = useLoadExampleWorkspace();
  if (examples.state !== "hasValue") {
    return <span>loading examples...</span>;
  }
  return (
    <div>
      {examples.contents.map((example, k) => (
        <div key={example.name}>
          <FileButton
            key={k}
            isFocused={false}
            onClick={() => loadExample(example)}
          >
            <div style={{ display: "flex", justifyContent: "space-between" }}>
              {/* <div
                style={{ width: 50, height: 50 }}
                dangerouslySetInnerHTML={{ __html: example.preview! }}
              ></div> */}
              {example.name}
            </div>
          </FileButton>
        </div>
      ))}
    </div>
  );
}
