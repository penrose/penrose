import * as React from "react";
import Layout from "../components/Layout";
import {
  EuiPageHeader,
  EuiCard,
  EuiFlexGroup,
  EuiFlexItem,
} from "@elastic/eui";

const IndexPage = () => {
  return (
    <Layout>
      <EuiPageHeader pageTitle="Penrose Docs" />
      <EuiFlexGroup gutterSize="l">
        <EuiFlexItem>
          <EuiCard
            layout="horizontal"
            title="Shapes"
            description="Shape definitions"
          />
        </EuiFlexItem>
      </EuiFlexGroup>
    </Layout>
  );
};

export default IndexPage;
