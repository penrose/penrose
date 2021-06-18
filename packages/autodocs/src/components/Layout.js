import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import { EuiPage, EuiPageSideBar, EuiPageBody, EuiSideNav } from "@elastic/eui";
import "@elastic/eui/dist/eui_theme_amsterdam_light.css";

const Layout = ({ children, itemId }) => {
  const { allShape } = useStaticQuery(graphql`
    query SidebarQuery {
      allShape {
        nodes {
          name
          id
        }
      }
    }
  `);
  const sideItems = allShape.nodes.map((item) => ({
    ...item,
    href: `/shape/${item.name}`,
    isSelected: itemId === item.id,
  }));
  return (
    <EuiPage paddingSize="none">
      <EuiPageSideBar paddingSize="m" sticky>
        <EuiSideNav
          items={[{ id: "shapes", name: "Shapes", items: sideItems }]}
        />
      </EuiPageSideBar>
      <EuiPageBody panelled>{children}</EuiPageBody>
    </EuiPage>
  );
};

export default Layout;
