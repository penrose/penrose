import * as React from "react";
import Layout from "../components/Layout";
import {
  EuiPageHeader,
  EuiCard,
  EuiFlexGroup,
  EuiFlexItem,
  EuiIcon,
  EuiPanel,
} from "@elastic/eui";
import { navigate } from "gatsby";

const IndexPage = () => {
  return (
    <Layout>
      <EuiPageHeader pageTitle="Penrose Docs" />
      <EuiPanel hasShadow={false}>
        <EuiFlexGroup gutterSize="l">
          <EuiFlexItem>
            <EuiCard
              layout="horizontal"
              title="Shapes"
              description="Shape definitions"
              icon={<EuiIcon size="xxl" type="heatmap" />}
              onClick={() => navigate("/shape")}
            />
          </EuiFlexItem>
        </EuiFlexGroup>
      </EuiPanel>
    </Layout>
  );
};

export default IndexPage;
