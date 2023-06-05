import { useRecoilValueLoadable } from "recoil";
import { exampleTriosState } from "../state/atoms.js";
import { useLoadExampleWorkspace } from "../state/callbacks.js";
import FileButton from "./FileButton.js";

export default function ExamplesBrowser() {
  const examples = useRecoilValueLoadable(exampleTriosState);
  const loadExample = useLoadExampleWorkspace();
  if (examples.state !== "hasValue") {
    return <span>loading examples...</span>;
  }

  const parser = new DOMParser();
  const serializer = new XMLSerializer();

  return (
    <div>
      {examples.contents.map((example, k) => {
        const svgDoc = parser.parseFromString(
          example.preview!,
          "image/svg+xml"
        );
        const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
        const svgNode = svgDoc.querySelector("svg")!;
        if (!(cropped === undefined)) {
          svgNode.setAttribute("viewBox", cropped!);
        }
        const croppedPreview = serializer.serializeToString(svgNode);

        return (
          <div key={example.name}>
            <FileButton
              key={k}
              isFocused={false}
              onClick={() => loadExample(example)}
            >
              <div style={{ display: "flex", justifyContent: "space-between" }}>
                {
                  <div
                    style={{ width: 50, height: 50 }}
                    dangerouslySetInnerHTML={{ __html: croppedPreview }}
                  ></div>
                }
                {example.name}
              </div>
            </FileButton>
          </div>
        );
      })}
    </div>
  );
}
