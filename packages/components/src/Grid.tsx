import { PathResolver } from "@penrose/core";

import styled from "styled-components";
import { Simple } from "./Simple";

const GridContainer = styled.main`
  flex-grow: 1;
  margin-left: "4rem";
`;

const GridContent = styled.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
`;

const Placeholder = styled.div``;

const PlaceholderText = styled.h3`
  font-family: "Roboto Mono";
  color: ${(props) => props.theme.primary};
`;

const Section = styled.div`
  margin: 0.5rem;
  width: 25rem;
  height: 25rem;
  border-color: ${(props) => props.theme.primary};
  border-width: 2px;
  border-style: solid;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
`;

const Header = styled.div`
  width: calc(100% - 0.75rem);
  height: 1.75rem;
  font-size: 1.25rem;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  padding: 0.5rem 0 0.5rem 0.75rem;
  vertical-align: text-bottom;
  color: ${(props) => props.theme.primary};
`;

const HeaderText = styled.div`
  color: ${(props) => props.theme.primary};
  vertical-align: text-bottom;
  font-family: monospace;
`;

type DiagramSource = {
  style: string;
  domain: string;
  substance: string;
  variation: string;
};

export default function Grid(props: {
  diagrams: DiagramSource[];
  header: (i: number) => string;
  imageResolver?: PathResolver;
}) {
  const { header, diagrams, imageResolver } = props;
  const innerContent = () => {
    return diagrams.map(({ substance, domain, style, variation }, i) => (
      <Section key={`gridbox-container-${i}`}>
        <Header>
          <HeaderText>{header(i)}</HeaderText>
        </Header>

        <div style={{ height: "calc(100% - 2.75rem)", position: "relative" }}>
          <Simple
            substance={substance}
            variation={variation}
            domain={domain}
            style={style}
            key={`gridbox-${i}`}
            name={`gridbox-${i}`}
            interactive={false}
            imageResolver={imageResolver}
          />
        </div>
      </Section>
    ));
  };
  const content =
    props.diagrams.length === 0 ? (
      <Placeholder>
        <PlaceholderText>
          {"(Generated diagrams will appear here)"}
        </PlaceholderText>
      </Placeholder>
    ) : (
      innerContent()
    );
  return (
    <GridContainer>
      <GridContent>{content}</GridContent>
    </GridContainer>
  );
}
