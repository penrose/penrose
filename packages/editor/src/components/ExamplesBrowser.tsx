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
        <FileButton
          key={k}
          isFocused={false}
          onClick={() => loadExample(example)}
        >
          {example.name}
        </FileButton>
      ))}
    </div>
  );
}
