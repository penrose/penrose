import queryString from "query-string";
import { useRecoilValueLoadable } from "recoil";
import styled from "styled-components";
import { TrioWithPreview, exampleTriosState } from "../state/atoms.js";
import { useLoadExampleWorkspace } from "../state/callbacks.js";
import FileButton from "./FileButton.js";

const parser = new DOMParser();
const serializer = new XMLSerializer();

const ExampleContainer = styled.div`
  margin: 0;
`;

const ExampleTab = styled.div`
  display: flex;
  justify-content: space-between;
  gap: 10px;
`;

const Example = ({
  example,
  loadExample,
  k,
}: {
  example: TrioWithPreview;
  loadExample: (t: TrioWithPreview) => Promise<void>;
  k: number;
}) => {
  const svgDoc = parser.parseFromString(example.preview!, "image/svg+xml");
  const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
  const svgNode = svgDoc.querySelector("svg")!;
  if (cropped !== undefined) {
    svgNode.setAttribute("viewBox", cropped!);
  }
  const croppedPreview = serializer.serializeToString(svgNode);

  return (
    <div key={example.name}>
      <FileButton
        key={k}
        isFocused={false}
        onClick={() => {
          loadExample(example);
          const query = queryString.stringify({
            examples: example.id,
          });
          // https://stackoverflow.com/questions/32828160/appending-parameter-to-url-without-refresh
          window.history.replaceState(null, "", `?${query}`);
        }}
      >
        <ExampleTab key={`example-tab-${k}`}>
          <div
            style={{ width: "50px", flexShrink: 0 }}
            dangerouslySetInnerHTML={{ __html: croppedPreview }}
          ></div>
          {example.name}
        </ExampleTab>
      </FileButton>
    </div>
  );
};

export default function ExamplesBrowser() {
  const examples = useRecoilValueLoadable(exampleTriosState);
  const loadExample = useLoadExampleWorkspace();
  if (examples.state !== "hasValue") {
    return <span>loading examples...</span>;
  }

  return (
    <ExampleContainer>
      {examples.contents.map((example, k) => (
        <Example
          example={example}
          loadExample={loadExample}
          k={k}
          key={`example-${k}`}
        ></Example>
      ))}
    </ExampleContainer>
  );
}
