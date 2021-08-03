import React from "react";
import styled from "styled-components";

const Section = styled.section`
  width: 30vw;
  height: 100vh;
  background-color: red;
  display: flex;
  flex-direction: column;
`;

export class Settings extends React.Component {
  constructor(props: {} | Readonly<{}>) {
    super(props);
  }
  render() {
    return <Section>Content</Section>;
  }
}
