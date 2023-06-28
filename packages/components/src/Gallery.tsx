import registry from "@penrose/examples/dist/registry";
import { useEffect, useState } from "react";
import styled from "styled-components";

export interface TrioWithPreview {
  id: string;
  name?: string;
  preview?: string;
}
const parser = new DOMParser();
const serializer = new XMLSerializer();

const Container = styled.div`
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  display: grid;
  align-items: center;
  justify-items: center;
  padding: 2em;
  grid-gap: 1.5em;
`;

const ExampleContainer = styled.div<{ dark?: boolean; shadow: string }>`
  width: 200px;
  height: 200px;
  padding: 1.5em;
  background-color: #fff;
  border-radius: 10px;
  box-shadow: ${(props) =>
    props.dark ? "" : "0px 3px 3px rgba(0, 0, 0, 0.1)"};
  transition: 0.3s;
  &:hover {
    box-shadow: ${(props) =>
      props.dark
        ? `0px 1px 2px 0px ${props.shadow},
          1px 2px 4px 0px ${props.shadow},
          2px 4px 8px 0px ${props.shadow},
          2px 4px 16px 0px ${props.shadow}`
        : "0px 10px 10px rgba(0, 0, 0, 0.2)"};

    transform: scale(1.05, 1.05);
  }
`;

const Example = ({
  example,
  ideLink,
  dark,
}: {
  ideLink: string;
  dark?: boolean;
  example: TrioWithPreview;
}) => {
  const svgDoc = parser.parseFromString(example.preview!, "image/svg+xml");
  const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
  const svgNode = svgDoc.querySelector("svg")!;
  if (cropped !== undefined) {
    svgNode.setAttribute("viewBox", cropped!);
  }
  const croppedPreview = serializer.serializeToString(svgNode);
  return (
    <a href={`${ideLink}?examples=${example.id}`} target="_blank">
      <ExampleContainer
        dark={dark}
        shadow={"rgb(46, 154, 216, .7)"}
        dangerouslySetInnerHTML={{ __html: croppedPreview }}
      ></ExampleContainer>
    </a>
  );
};

export default ({ ideLink, dark }: { ideLink: string; dark?: boolean }) => {
  let [examples, setExamples] = useState<TrioWithPreview[]>([]);
  useEffect(() => {
    const load = async () => {
      const entries = registry.entries();
      for (const [id, meta] of entries) {
        if (meta.trio && meta.gallery) {
          const svg = await fetch(
            encodeURI(
              `https://raw.githubusercontent.com/penrose/penrose/ci/refs/heads/main/${id}.svg`
            )
          );
          if (svg.ok) {
            const preview = await svg.text();

            // crop the SVG
            const svgDoc = parser.parseFromString(preview, "image/svg+xml");
            const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
            const svgNode = svgDoc.querySelector("svg")!;
            if (cropped !== undefined) {
              svgNode.setAttribute("viewBox", cropped!);
            }
            const croppedPreview = serializer.serializeToString(svgNode);

            const trio = { id, preview: croppedPreview };
            setExamples((ex) => [...ex, trio]);
          }
        }
      }
    };
    load();
  }, []);
  return (
    <Container>
      {examples.map((e) => {
        return <Example example={e} ideLink={ideLink} dark={dark}></Example>;
      })}
    </Container>
  );
};
