import { EnhanceAppContext } from "vitepress";
import DefaultTheme from "vitepress/theme";
import CirclePackingDisjoint from "../../src/bloom-examples/CirclePackingDisjoint.vue";
import CirclePackingEqual from "../../src/bloom-examples/CirclePackingEqual.vue";
import CirclePackingPadded from "../../src/bloom-examples/CirclePackingPadded.vue";
import Eigen from "../../src/bloom-examples/Eigen.vue";
import Rays from "../../src/bloom-examples/Rays.vue";
import Reflection from "../../src/bloom-examples/Reflection.vue";
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

    // for some reason, bloom blog breaks with script tag imports
    ctx.app.component("Eigen", Eigen);
    ctx.app.component("Reflection", Reflection);
    ctx.app.component("CirclePackingDisjoint", CirclePackingDisjoint);
    ctx.app.component("CirclePackingPadded", CirclePackingPadded);
    ctx.app.component("CirclePackingEqual", CirclePackingEqual);
    ctx.app.component("Rays", Rays);
  },
};
