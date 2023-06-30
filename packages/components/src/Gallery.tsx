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

// crop the SVG if necessary, i.e. if the
// cropped view box is smaller than the current view box
const cropSVG = (svg: string) => {
  const svgDoc = parser.parseFromString(svg, "image/svg+xml");
  const cropped = svgDoc.querySelector("croppedViewBox")?.innerHTML;
  const svgNode = svgDoc.querySelector("svg")!;
  const viewBox = svgNode.getAttribute("viewBox");
  if (cropped && viewBox) {
    const viewBoxNums = svgNode.viewBox.baseVal;
    const croppedNums = cropped.split(/\s+|,/);

    const croppedWidth = parseFloat(croppedNums[3]);
    const croppedHeight = parseFloat(croppedNums[4]);
    const viewBoxWidth = viewBoxNums.width;
    const viewBoxHeight = viewBoxNums.height;

    // if area of cropped view box is leq area of current view box
    // then set the view box to the cropped view box
    if (
      Math.abs(croppedWidth * croppedHeight) <
      Math.abs(viewBoxWidth * viewBoxHeight)
    ) {
      svgNode.setAttribute("viewBox", cropped);
    }
  }

  return serializer.serializeToString(svgNode);
};

const Example = ({
  example,
  ideLink,
}: {
  ideLink: string;
  example: TrioWithPreview;
}) => {
  const croppedPreview = cropSVG(example.preview!);
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
            const preview = await svg.text();
            const croppedPreview = cropSVG(preview);
            const trio = { id, preview: croppedPreview };
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
