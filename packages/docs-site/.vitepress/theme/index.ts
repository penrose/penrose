import { EnhanceAppContext } from "vitepress";
import DefaultTheme from "vitepress/theme";
import Layout from "../../src/components/Layout.vue";
import BlogHome from "./BlogHome.vue";
import BlogMeta from "./BlogMeta.vue";
import "./custom.css";

export default {
  extends: DefaultTheme,
  Layout,
  enhanceApp(ctx: EnhanceAppContext) {
    // register your custom global components
    ctx.app.component("BlogHome", BlogHome);
    ctx.app.component("BlogMeta", BlogMeta);
  },
};
