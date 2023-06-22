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
  width: 200px;
  height: 200px;
  padding: 1.5em;
  background-color: white;
  box-shadow: 0px 3px 3px rgba(0, 0, 0, 0.1);
  transition: 0.3s;
  &:hover {
    box-shadow: 0px 20px 40px rgba(0, 0, 0, 0.2);
    transform: scale(1.05, 1.05);
  }
`;

const Example = ({
  example,
  ideLink,
}: {
  ideLink: string;
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
      <Container
        dangerouslySetInnerHTML={{ __html: croppedPreview }}
      ></Container>
    </a>
  );
};

export default ({ ideLink }: { ideLink: string }) => {
  let [examples, setExamples] = useState<TrioWithPreview[]>([]);
  useEffect(() => {
    const load = async () => {
      for (const [id, meta] of registry.entries()) {
        if (meta.trio && meta.gallery) {
          const svg = await fetch(
            encodeURI(
              `https://raw.githubusercontent.com/penrose/penrose/ci/refs/heads/main/${id}.svg`
            )
          );
          if (svg.ok) {
            const trio = { id, preview: await svg.text() };
            setExamples((ex) => [...ex, trio]);
          }
        }
      }
    };
    load();
  }, []);
  return (
    <div style={{ display: "flex", flexWrap: "wrap" }}>
      {examples.map((e) => {
        return <Example example={e} ideLink={ideLink}></Example>;
      })}
    </div>
  );
};
