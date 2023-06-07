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
  box-shadow: 0px 3px 3px rgba(0, 0, 0, 0.1);
  transition: 0.3s;
  &:hover {
    box-shadow: 0px 20px 40px rgba(0, 0, 0, 0.2);
    transform: scale(1.05, 1.05);
  }
`;

const Example = ({ example }: { example: TrioWithPreview }) => {
  const svgDoc = parser.parseFromString(example.preview!, "image/svg+xml");
  const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
  const svgNode = svgDoc.querySelector("svg")!;
  if (cropped !== undefined) {
    svgNode.setAttribute("viewBox", cropped!);
  }
  const croppedPreview = serializer.serializeToString(svgNode);
  return (
    <a
      href={`http://127.0.0.1:3000/try/?examples=${example.id}`}
      target="_blank"
    >
      <Container
        dangerouslySetInnerHTML={{ __html: croppedPreview }}
      ></Container>
    </a>
  );
};

export default () => {
  let [examples, setExamples] = useState<TrioWithPreview[]>([]);
  useEffect(() => {
    const load = async () => {
      const trios: TrioWithPreview[] = [];
      for (const [id, meta] of registry.entries()) {
        if (meta.trio && meta.gallery) {
          const svg = await fetch(
            encodeURI(
              `https://raw.githubusercontent.com/penrose/penrose/ci/refs/heads/main/${id}.svg`
            )
          );
          if (svg.ok) {
            trios.push({ id, preview: await svg.text() });
          }
        }
      }
      setExamples(trios);
    };
    load();
  });
  return (
    <div style={{ display: "flex", flexWrap: "wrap" }}>
      {examples.map((e) => {
        return <Example example={e}></Example>;
      })}
    </div>
  );
};
