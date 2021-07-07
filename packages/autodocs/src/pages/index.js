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
import { navigate, Link } from "gatsby";

const IndexPage = () => {
  return (
    <Layout title={"Home"}>
      <EuiPageHeader pageTitle="Penrose Docs" />
      <EuiPanel hasShadow={false}>
        <EuiFlexGroup gutterSize="l">
          <EuiFlexItem>
            <Link to="/shape">
              <EuiCard
                layout="horizontal"
                title="Shapes"
                description="Shape definitions"
                icon={<EuiIcon size="xxl" type="heatmap" />}
                onClick={() => navigate("/shape")}
              />
            </Link>
          </EuiFlexItem>
        </EuiFlexGroup>
      </EuiPanel>
    </Layout>
  );
};

export default IndexPage;
