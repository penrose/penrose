import React, { useState } from "react";
import { useStaticQuery, graphql, Link } from "gatsby";
import {
  EuiPage,
  EuiPageSideBar,
  EuiPageBody,
  EuiSideNav,
  EuiHeader,
  EuiHeaderLogo,
} from "@elastic/eui";
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
    isSelected: itemId === item.id,
    renderItem: ({ children }) => {
      return (
        <Link
          to={`/shape/${item.name}`}
          className="euiSideNavItemButton euiSideNavItemButton--isClickable"
          activeClassName="euiSideNavItemButton-isSelected"
        >
          {children}
        </Link>
      );
    },
  }));
  const [isSideNavOpenOnMobile, setisSideNavOpenOnMobile] = useState(false);

  const toggleOpenOnMobile = () => {
    setisSideNavOpenOnMobile((val) => !val);
  };
  return (
    <>
      <EuiHeader
        position="fixed"
        sections={[
          {
            items: [<EuiHeaderLogo href="/">Penrose</EuiHeaderLogo>],
          },
        ]}
      />
      <EuiPage paddingSize="s" style={{ marginTop: "48px" }}>
        <EuiPageSideBar paddingSize="m" sticky>
          <EuiSideNav
            toggleOpenOnMobile={toggleOpenOnMobile}
            isOpenOnMobile={isSideNavOpenOnMobile}
            mobileTitle="Menu"
            items={[{ id: "shapes", name: "Shapes", items: sideItems }]}
          />
        </EuiPageSideBar>
        <EuiPageBody panelled>{children}</EuiPageBody>
      </EuiPage>
    </>
  );
};

export default Layout;
