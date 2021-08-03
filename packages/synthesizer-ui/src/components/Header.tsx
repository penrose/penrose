import styled from "styled-components";
import React from "react";

const Section = styled.section`
  display: flex;
  flex-direction: row;
  width: calc(100%-2rem);
  justify-content: space-between;
  padding-left: 1rem;
  padding-right: 1rem;
`;

const BtnContainer = styled.section`
  display: flex;
  flex-direction: row;
  width: 10rem;
  justify-content: flex-end;
`;

const H1 = styled.h1``;

const Button = styled.button`
  display: inline-block;
  color: gray;
  font-size: 1rem;
  margin: 1rem;
  padding: 0.25rem 1rem;
  border: 2px solid gray;
  border-radius: 0.5rem;
  display: block;
`;

const ExportButton = styled(Button)`
  background-color: purple;
  border: none;
  color: white;
`;
export function Header() {
  return (
    <Section>
      <div className="flex-1 min-w-0">
        <H1>Edgeworth</H1>
      </div>
      <BtnContainer>
        <span className="hidden sm:block">
          <ExportButton>Export</ExportButton>
        </span>

        <span className="sm:ml-3">
          <Button>Refresh</Button>
        </span>
      </BtnContainer>
    </Section>
  );
}
