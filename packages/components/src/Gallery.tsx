import { useEffect, useState } from "react";
import styled from "styled-components";
import { cropSVG } from "./util.js";

export interface TrioWithPreview {
  id: string;
  name?: string;
  preview?: string;
}

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
  return (
    <a href={`${ideLink}?examples=${example.id}`} target="_blank">
      <ExampleContainer
        dark={dark}
        shadow={"rgb(46, 154, 216, .7)"}
        dangerouslySetInnerHTML={{ __html: example.preview ?? "" }}
      ></ExampleContainer>
    </a>
  );
};

export default ({
  trios,
  ideLink,
  dark,
}: {
  trios: string[];
  ideLink: string;
  dark?: boolean;
}) => {
  let [examples, setExamples] = useState<TrioWithPreview[]>(
    trios.map((id) => ({ id }))
  );
  useEffect(() => {
    const load = async () => {
      for (const id of trios) {
        const svg = await fetch(
          encodeURI(
            `https://raw.githubusercontent.com/penrose/penrose/ci/refs/heads/main/${id}.svg`
          )
        );
        if (svg.ok) {
          const preview = await svg.text();
          const croppedPreview = cropSVG(preview);
          const trio = { id, preview: croppedPreview };
          setExamples((ex) =>
            ex.map((e) =>
              e.id === id ? { ...trio, preview: croppedPreview } : e
            )
          );
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
