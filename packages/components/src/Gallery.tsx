import registry from "@penrose/examples/dist/registry";
import { useEffect, useState } from "react";

export interface TrioWithPreview {
  id: string;
  name?: string;
  preview?: string;
}
const parser = new DOMParser();
const serializer = new XMLSerializer();
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
      <div
        style={{ width: 200, height: 200, padding: "1em" }}
        dangerouslySetInnerHTML={{ __html: croppedPreview }}
      ></div>
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
