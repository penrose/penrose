import styled from "styled-components";
import React from "react";

const Section = styled.section`
  display: flex;
  flex-direction: row;
  width: calc(100%-2rem);
  justify-content: space-between;
  padding-left: 1rem;
  padding-right: 1rem;
  border-bottom: 1px solid gray;
`;

const BtnContainer = styled.section`
  display: flex;
  flex-direction: row;
  width: 10rem;
  justify-content: flex-end;
`;

const H1 = styled.h1``;

const Btn = styled.button`
  display: inline-block;
  color: gray;
  font-size: 1rem;
  margin: 1rem;
  width: 4.5rem;
  height: 2rem;
  padding: 0.25rem 1rem;
  border: 2px solid gray;
  border-radius: 0.25rem;
  display: flex;
  justify-content: center;
  align-items: center;
`;

const ExportBtn = styled(Btn)`
  background-color: purple;
  border: none;
  color: white;
`;

export interface HeaderProps {
  update: () => {};
}

export class Header extends React.Component<HeaderProps> {
  constructor(props: HeaderProps) {
    super(props);
  }

  onClick() {
    this.props.update();
  }

  render() {
    return (
      <Section>
        <H1>Edgeworth</H1>
        <BtnContainer>
          <ExportBtn>Export</ExportBtn>
        </BtnContainer>
      </Section>
    );
  }
}
